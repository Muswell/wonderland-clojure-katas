(ns alphabet-cipher.coder)

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn prev-letter [letter]
  (let [range (char-range \a letter)]
    (nth range (- (.indexOf range letter) 1))))

(defn char-row [letter]
  (if (= \a letter)
    (char-range \a \z)
    (concat (char-range letter \z) (char-range \a (prev-letter letter)))))

(defn encode-char [secret letter]
  (nth (char-row letter) (.indexOf (char-row \a) secret)))

(defn decode-char [secret letter]
  (let [alphabet (char-range \a \z) col (.indexOf alphabet secret)]
    (loop [i \a]
      (if (= col (.indexOf (char-row i) letter) col)
        i
        (recur (char (inc (int i))))))))

(defn decipher-char [cipher letter]
  (nth (char-row \a) (.indexOf (char-row letter) cipher)))

(defn pad-keyword [keyword len]
  (take len (apply concat (repeat keyword))))

(defn process-message [fn keyword message]
  (let [secret (pad-keyword keyword (count message))]
    (apply str (map fn secret (char-array message)))))

(defn encode [keyword message]
  (process-message encode-char keyword message))

(defn decode [keyword message]
  (process-message decode-char keyword message))

(defn decipher [cipher message]
  (let [secret (process-message decipher-char cipher message)]
    (loop [i 1]
      (let [keyword (subs secret 0 i)]
        (if (= cipher (encode keyword message))
          keyword
          (recur (inc i)))))))
