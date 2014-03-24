(ns ^{:doc "Implementation of http://corewar.co.uk/icws94.txt."}
  marston.core)

(def MODIFIERS #{:A :B :AB :BA :F :X :I})
(def MODES #{:# :$ :at :< :> :* :a< :a>}) 

(defrecord Instruction [opcode
                        modifier
                        a-mode
                        a-number
                        b-mode
                        b-number])

(defn inst
  "Create an instruction. Arglists:
[op anumber bnumber]
[op modifier anumber bnumber]
[op amode anumber bmode bnumber]
[op modifier amode anumber bmode bnumber]
modifer defaults to :I, modes to :$."
  ([op an bn]
     (Instruction. op :I :$ an :$ bn))
  ([op mod an bn]
     {:pre [(MODIFIERS mod)]}
     (Instruction. op mod :$ an :$ bn))
  ([op am an bm bn]
     {pre [(MODES am) (MODES bm)]}
     (Instruction. op :I am an bm bn))
  ([op mod am an bm bn]
     {:pre [(MODIFIERS mod) (MODES am) (MODES bm)]}
     (Instruction. op mod am an bm bn)))

(def SPECS {:icws86 {:coresize      8192
                     :maxcycles     10000
                     :initial       (inst :dat :F :# 0 :# 0)
                     :maxlength     300
                     :maxprocesses  64
                     :mindistance   300
                     :readlimit     8192
                     :separation    :random
                     :warriors      2
                     :writelimit    8192}
            :koth {:coresize      8000
                   :maxcycles     80000
                   :initial       (inst :dat :F :$ 0 :$ 0)
                   :maxlength     100
                   :maxprocesses  8000
                   :mindistance   100
                   :readlimit     8000
                   :separation    :random
                   :warriors      2
                   :writelimit    8000}})

;; Helpers for the modular maths

(defn wrapper
  "Return fn to wrap pointer to approriate range. Assumes limit is
  factor of coresize."
  [coresize limit]
  {:pre [(zero? (mod coresize limit))]}
  (fn [ptr]
    (let [result (mod ptr limit)]
      (if (> result (/ limit 2))
        (+ result (- coresize limit))
        result))))

(defn shifter [size]
  (fn [pc offset]
    (mod (+ pc offset) size)))

(defn decrementer [size]
  (fn [pc]
    (mod (dec (+ size pc)) size)))

(defn incrementer [size]
  (fn [pc]
    (mod (inc pc) size)))

(defn state
  "Initialise state to blank core as described by the core description
  and empty process-queue. coredesc can be map or :koth or :icws86."
  [coredesc]
  (let [coredesc (if (keyword? coredesc) (coredesc SPECS) coredesc)
        {:keys [coresize
                maxcycles
                initial
                maxlength
                maxprocesses
                mindistance
                readlimit
                separation
                warriors
                writelimit]} coredesc]
    {:core (vec (repeat coresize nil)) ; intialise to nil and lazily
                                       ; to :initial
     :coredesc coredesc
     :process-queue {}
     :cycle 0}))

(defn status [{:keys [process-queue cycle coredesc]}]
  (let [alive (filter (fn [[k v]] (not (empty? v))) process-queue)]
    (case (count alive)
      0 [:drawn]
      1 [:won (ffirst alive)]
      (if (>= cycle (:maxcycles coredesc))
        [:drawn]
        [:ongoing]))))



(def metrics
  "Returns mod arithmetic functions based on the coredesc. e.g.
`(let [[read-wrap write-wrap shift decrement increment] (metrics (:coredesc state))] ...)`"
  (memoize 
   (fn metrics
     [{n :coresize
       rlimit :readlimit
       wlimit :writelimit}]
     [(wrapper n rlimit) (wrapper n wlimit) (shifter n) (decrementer n) (incrementer n)])))

(defn load
  "Load the instruction at the specified position, returning default
instruction if it's uninitialised."
  [{:keys [core coredesc]} pc ptr]
  (let [[_ _ shift _ _] (metrics coredesc)]
    (or (nth core (shift pc ptr))
        (:initial coredesc))))

;; Functions for processing the address resolution and attendant
;; pre-decrements / post-increments. These return operations on core
;; and "registers" and accept precomputed metric functions as first
;; arg.

(defn decrements [[_ _ shift decrement _] field]
  {:pre [(#{:a-number :b-number} field)]}

  (fn [{:keys [core coredesc]} pc rp wp pip]
    {:pre [(vector? core) coredesc]}
    (let [state+ (update-in state
                            [:core (shift pc wp)]
                            (fnil #(update-in % [field] decrement)
                                  (:initial coredesc)))]
      [state+ pc rp wp pip])))

(defn record-increments [[_ _ shift _ _]]
  
  (fn [{core :core} pc rpa wpa pip]
    {:pre [(vector? core)]}
    [state pc rpa wpa (shift pc wpa)]))

(defn follow-indirections [[read-wrap write-wrap shift _ _] field]
  {:pre [(#{:a-number :b-number} field)]}

  (fn [{core :core :as state} pc rp wp pip]
    {:pre [(vector? core)]}
    [state
     pc
     (read-wrap (+ rp (field (load state pc rp))))
     (write-wrap (+ wp (field (load state pc wp))))
     pip]))

(defn increments [[_ _ _ _ increment] field]
  {:pre [(#{:a-number :b-number} field)]}
  
  (fn [{:keys [core coredesc]} pc rp wp pip]
    {:pre [(vector? core)]}
    (let [state+ (update-in state
                            [:core pip]
                            (fnil #(update-in % [field] increment)
                                  (:initial coredesc)))]
      [state+ pc rp wp pip])))

(defn resolve-references
  "Resolve the approproate read and write pointers from instruction
ir, updating core where necessary. mode-key is :a-mode or :b-mode and
number-key is :a-number or :b-number.
Returns [state read-pointer write-pointer]."
  [[read-wrap write-wrap :as metrics] {core :core :as state} pc mode-key number-key]
  {:pre [(#{:a-mode :b-mode} mode-key) (#{:a-number :b-number} number-key)]}
  (let [ir (load state pc 0)
        mode (mode-key ir)]
    (if (= :# mode)
      
      [state 0 0]                       ; immediate mode self-addresses
      
      (cond->> [state pc (read-wrap (number-key ir)) (write-wrap (number-key ir)) nil]

                                        ; apply any pre-decrements in the core
               (= :a< mode) (apply (decrements metrics :a-number))
               (= :< mode) (apply (decrements metrics :b-number))

                                        ; record post-increments for later
               (#{:> :a>} mode) (apply (record-increments metrics))

                                        ; follow indirections to update pointers
               (#{:* :a< :a>} mode) (apply (follow-indirections metrics :a-number))
               (#{:at :< :>} mode) (apply (follow-indirections metrics :b-number))

                                        ; apply the post-increments
               (= :a> mode) (apply (increments metrics :a-number))
               (= :> mode) (apply (increments metrics :b-number))

                                        ; select out the changed data (core and pointers)
               true ((fn [[state pc rp wp pip]] [state rp wp]))))))

;; opcode execution

(defmulti exec-opcode
  "Process the instruction and return updated state and program
  counters to reinsert to the process queue. rpa / rpb / wpa / wpb are
  offsets from pc and need adding to pc before dereferencing. (There
  seems to be a bug in the spec here.)"
  (fn [state pc ir rpa ira rpb irb wpa wpb]
    (:opcode ir)))

(defmethod exec-opcode :dat [state pc ir rpa ira rpb irb wpa wpb]
  [state []])

(defmethod exec-opcode :mov [state pc ir rpa ira rpb irb wpa wpb]
  (let [[_ _ shift _ increment] (metrics (:coredesc state))
        mov (fn [src dst state] (assoc-in state
                                         [:core (shift pc wpb)]
                                         (let [wrb (load state pc wpb)]
                                           (assoc wrb dst (src ira)))))
        state (case (:modifier ir)
                :A  (mov :a-number :a-number state)
                :B  (mov :b-number :b-number state)
                :AB (mov :a-number :b-number state)
                :BA (mov :b-number :a-number state)
                :F  (mov :a-number :a-number (mov :b-number :b-number state))
                :X  (mov :a-number :b-number (mov :b-number :a-number state))
                :I  (assoc-in state [:core (shift pc wpb)] ira))]
    [state [(increment pc)]]))

(defn lift [op]
  (fn [state pc ir rpa ira rpb irb wpa wpb]
    (let [[_ _ shift _ increment] (metrics (:coredesc state))
          upd (fn [dst a-field b-field st]
                (assoc-in st
                          [:core (shift pc wpb)]
                          (let [wrb (load state pc wpb)]
                            (assoc wrb dst (op (a-field ira) (b-field irb))))))
          state (case (:modifier ir)
                  :A (upd :a-number :a-number :a-number state)
                  :B (upd :b-number :b-number :b-number state)
                  :AB (upd :b-number :a-number :b-number state)
                  :BA (upd :a-number :b-number :a-number state)
                  :F (->> state
                          (upd :a-number :a-number :a-number)
                          (upd :b-number :b-number :b-number))
                  :I (->> state
                          (upd :a-number :a-number :a-number)
                          (upd :b-number :b-number :b-number))
                  :X (->> state
                          (upd :b-number :a-number :b-number)
                          (upd :a-number :b-number :a-number)))]
      [state [(increment pc)]])))

(defmethod exec-opcode :add [state pc ir rpa ira rpb irb wpa wpb]
  ((lift +) state pc ir rpa ira rpb irb wpa wpb))

(defmethod exec-opcode :sub [state pc ir rpa ira rpb irb wpa wpb]
  ((lift (fn [l r] (- (+ l (:coresize (:coredesc state))) r))) state pc ir rpa ira rpb irb wpa wpb))

(defmethod exec-opcode :mul [state pc ir rpa ira rpb irb wpa wpb]
  ((lift *) state pc ir rpa ira rpb irb wpa wpb))

(defn lift' [op]
  (fn [state pc ir rpa ira rpb irb wpa wpb]
    (let [[_ _ shift _ increment] (metrics (:coredesc state))
          upd (fn [dst a-field b-field st]
                (when (not (zero? (b-field ira)))
                  (assoc-in st
                            [:core (shift pc wpb)]
                            (let [wrb (load state pc wpb)]
                              (assoc wrb dst (op (a-field ira) (b-field irb)))))))
          state+ (case (:modifier ir)
                   :A (upd :a-number :a-number :a-number state)
                   :B (upd :b-number :b-number :b-number state)
                   :AB (upd :b-number :a-number :b-number state)
                   :BA (upd :a-number :b-number :a-number state)
                   :F (some->> state
                               (upd :a-number :a-number :a-number)
                               (upd :b-number :b-number :b-number))
                   :I (some->> state
                               (upd :a-number :a-number :a-number)
                               (upd :b-number :b-number :b-number))
                   :X (some->> state
                               (upd :b-number :a-number :b-number)
                               (upd :a-number :b-number :a-number)))]
      (if state+
        [state+ [(increment pc)]]
        [state []]))))

(defmethod exec-opcode :div [state pc ir rpa ira rpb irb wpa wpb]
  ((lift' /) state pc ir rpa ira rpb irb wpa wpb))

(defmethod exec-opcode :mod [state pc ir rpa ira rpb irb wpa wpb]
  ((lift' mod) state pc ir rpa ira rpb irb wpa wpb))

(defmethod exec-opcode :jmp [state pc ir rpa ira rpb irb wpa wpb]
  (let [[_ _ shift _ _] (metrics (:coredesc state))]
    [state [(shift pc rpa)]]))

(defn jump? [modifier irb predicate]
  (or 
   (and (#{:A :BA} modifier) (zero? (:a-number irb)))
   (and (#{:B :AB} modifier) (zero? (:b-number irb)))
   (and (#{:F :X :I} modifier) (every? zero? [(:a-number irb) (:b-number irb)]))))

(defmethod exec-opcode :jmz [state pc ir rpa ira rpb irb wpa wpb]
  (let [[_ _ _ _ increment] (metrics (:coredesc state))]
    [state (if (jump? (:modifier ir) irb zero?) [rpa] [(increment pc)])]))

(defmethod exec-opcode :jmn [state pc ir rpa ira rpb irb wpa wpb]
  (let [[_ _ _ _ increment] (metrics (:coredesc state))]
    [state (if (jump? (:modifier ir) irb (complement zero?)) [rpa] [(increment pc)])]))

(defmethod exec-opcode :djn [state pc ir rpa ira rpb irb wpa wpb]
  (let [[_ _ shift increment decrement] (metrics (:coredesc state))
        initial (:initial (:coredesc state))
        modifier (:modifier ir)
        state (cond-> state
                      (#{:A :BA :F :X :I} modifier) (update-in [:core (shift pc wpb)]
                                                               (fnil #(update-in % :a-number decrement)
                                                                     initial))
                      (#{:B :AB :F :X :I} modifier) (update-in [:core (shift pc wpb)]
                                                               (fnil #(update-in % :b-number decrement)
                                                                     initial)))
        irb (cond-> irb
                    (#{:A :BA :F :X :I} modifier) (update-in [:a-number] dec)
                    (#{:B :AB :F :X :I} modifier) (update-in [:b-number] dec))]
    [state (if (jump? modifier irb (complement zero?)) [rpa] [(increment pc)])]))

(defmethod exec-opcode :cmp [state pc ir rpa ira rpb irb wpa wpb]
  (let [[_ _ _ _ increment] (metrics (:coredesc state))
        modifier (:modifier ir)
        skip? (case modifier
                :A (= (:a-number ira) (:a-number irb))
                :B (= (:b-number ira) (:b-number irb))
                :AB (= (:a-number ira) (:b-number irb))
                :BA (= (:b-number ira) (:a-number ira))
                :F (and (= (:a-number ira) (:a-number irb))
                        (= (:b-number ira) (:b-number irb)))
                :X (and (= (:b-number ira) (:a-number irb))
                        (= (:a-number ira) (:b-number irb)))
                :I (= ira irb))]
    [state (if skip? (increment (increment pc)) (increment pc))]))

(defmethod exec-opcode :slt [state pc ir rpa ira rpb irb wpa wpb]
  (let [[_ _ _ _ increment] (metrics (:coredesc state))
        modifier (:modifier ir)
        skip? (case modifier
                :A (< (:a-number ira) (:a-number irb))
                :B (< (:b-number ira) (:b-number irb))
                :AB (< (:a-number ira) (:b-number irb))
                :BA (< (:b-number ira) (:a-number ira))
                :F (and (< (:a-number ira) (:a-number irb))
                        (< (:b-number ira) (:b-number irb)))
                :X (and (< (:b-number ira) (:a-number irb))
                        (< (:a-number ira) (:b-number irb)))
                :I (and (< (:a-number ira) (:a-number irb))
                        (< (:b-number ira) (:b-number irb))))]
    [state (if skip? (increment (increment pc)) (increment pc))]))

(defmethod exec-opcode :spl [state pc ir rpa ira rpb irb wpa wpb]
  (let [[_ _ _ _ increment] (metrics (:coredesc state))]
    [state [(increment pc) rpa]]))

(defn exec1 [{:keys [core coredesc] :as state} pc]
  {:pre [(>= pc 0) (< pc (:coresize coredesc))]}
  (let [        
        ;; resolve references and pre / post increment base on modes
        ;;
        [read-wrap write-wrap shift :as metrics] (metrics coredesc)
        [state rpa wpa] (resolve-references metrics state pc :a-mode :a-number)
        [state rpb wpb] (resolve-references metrics state pc :b-mode :b-number)

        ir (load state pc 0)
        ira (load state pc rpa)
        irb (load state pc rpb)

        ;; pass state and registers to opcode, get new state and
        ;; processes for queue
        [state+ pcs] (exec-opcode state pc ir rpa ira rpb irb wpa wpb)]
    [state+ pcs]))

(defn select-non-overlapping-starts
  "For warriors of length1 and length2 in core of size coresize select
random start points so they do not overlap."
  [{:keys [coresize mindistance separation]} & lengths]
  (if (= :random separation)
    (let [n (count lengths)
          minseparations (map #(max % mindistance) lengths)
          make-candidate (fn [] (take n (repeatedly #(int (rand coresize)))))
          acceptable? (fn [starts]
                        (let [separations (->> (cycle starts)
                                               (take (inc (count starts)))
                                               (partition 2 1)
                                               (map #(mod (apply - %) coresize)))]
                          (every? #(apply > %) (map vector separations minseparations))))]
      (first (drop-while (complement acceptable?) (repeatedly make-candidate))))
    (map (partial * separation) (range 0 (* (count lengths))))))

(defn initialise-warrior
  "Initialise a warrior into the core."
  [state {:keys [name start instructions]} ptr]
  (let [size (:coresize (:coredesc state))
        replacements (zipmap (range ptr (+ ptr (count instructions)))
                             instructions)]
    (-> state
        (update-in [:core]
                   (fn [core]
                     (reduce (fn [c [i v]] (assoc c (mod i size) v)) core replacements)))
        (assoc-in [:process-queue name] [(mod (+ ptr start) size)]))))

(defn step [{:keys [core process-queue] :as state} warrior-name]
  (if-let [pc (peek (process-queue warrior-name))]
    (let [[state+ pcs] (exec1 state pc)]
         (-> state+
             (update-in [:process-queue warrior-name] pop)
             (update-in [:process-queue warrior-name] into pcs)
             (update-in [:cycle] inc)))
    (update-in state [:cycle] inc)))

(defn battle
  "Battle warrior1 and warrior2 (each seqs of Instruction) in 8000 core from
random start points."
  [warrior1 warrior2]
  (let [state (state :koth)
        [w1-start w2-start] (select-non-overlapping-starts (:coredesc state)
                                                           (count (:instructions warrior1))
                                                           (count (:instructions warrior2)))
        state (-> state
                  (initialise-warrior warrior1 w1-start)
                  (initialise-warrior warrior2 w2-start))
        turns (cycle (map :name [warrior1 warrior2]))]

    (first (drop-while #(= (status %) [:ongoing]) (reductions step state turns)))))

(def IMP {:name "imp"
          :instructions [(inst :mov 0 1)]
          :start 0})

(def DWARF {:name "dwarf"
            :instructions [(inst :dat :F :# 0 :# 0)
                           (inst :add :AB :# 4 :$ -1)
                           (inst :mov :AB :# 0 :at -2)
                           (inst :jmp :A -2 0)]
            :start 1})

