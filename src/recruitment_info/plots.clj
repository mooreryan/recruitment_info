;; Copyright 2014 Ryan Moore

;; This file is part of recruitment_info.

;; recruitment_info is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; recruitment_info is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with recruitment_info.  If not, see
;; <http://www.gnu.org/licenses/>.

(ns recruitment_info.plots
  (:require [clojure.set :as set])
  (:use [clojure.java.shell :only [sh]]))

(defn r [r-string]
  (let [{:keys [exit out err]} (apply sh ["Rscript" "-e" r-string])] 
    (if (zero? exit) out (do (println err) (System/exit 3)))))

(defn r-script [fname]
  (let [{:keys [exit out err]} (apply sh ["Rscript" fname])] 
    (if (zero? exit) out (do (println err) (System/exit 3)))))


#_(defn plot-cov
  "cov-vector is a vector like so [[2 3 4] [3 4 5 6] [5 6 7 8
  9]]. TODO: Consider instead writing everything to one massive R
  script and then calling this once instead of once for every graph we
  need."
  [cov-vector ref-name ref-len outdir id]
  (let [outd (clojure.string/replace outdir #"/$" "")
        outf (format "%s/tmp.2394230498397.r" outd)
        freqs (frequencies (flatten cov-vector))
        xs (range 1 (inc ref-len))
        ;; fill ys with zeros for areas of no coverage
        ys (map (fn [x] (if (contains? freqs x) (freqs x) 0)) xs)
        [x y] (map #(format "c(%s)" %) 
                   [(clojure.string/join ", " xs) 
                    (clojure.string/join ", " ys)])]
    (spit outf
          (format (str (format "pdf('%s/%s_cov_%s.pdf', width=8, height=5);" 
                            outd ref-name id ref-name id)
                    "plot(x=%s, y=%s, main='%s %s', xlab='Position', ylab='Coverage', "
                    "type='l');"
                    "invisible(dev.off());") 
               x y ref-name id ref-len))
    (r-script outf)))

(defn get-xy-strings [cov-vecs]
  (loop [v cov-vecs c 0 strs []] 
    (if (seq (first v)) 
      (recur (rest v) 
             (inc c) 
             (conj strs (format "x=c(%s), y=c(%s)" 
                                (clojure.string/join ", " (first v)) 
                                (clojure.string/join ", " 
                                                     (repeat 
                                                      (count (first v)) c))))) 
      strs)))

(defn foo [cov-vecs ys]
  (loop [v cov-vecs last-cov-vec [] ys ys c 1 strs []] 
    (if (seq (first v))
      (let [intersect (seq (set/intersection (set (first v)) (set last-cov-vec)))
            x-string (clojure.string/join ", " (first v))
            y-string (clojure.string/join ", " (repeat (count (first v))
                                                       ; get cov at starting x posn
                                                       (nth ys
                                                            (inc (first (first v))))))]
        (recur (rest v)
               (first v)
               (rest ys)
               (if intersect (inc c) c)
               (conj strs (format "x=c(%s), y=c(%s)" x-string y-string)))) 
      strs)))

(defn plot-cov
  "cov-vector is a vector like so [[2 3 4] [3 4 5 6] [5 6 7 8
  9]]. TODO: Consider instead writing everything to one massive R
  script and then calling this once instead of once for every graph we
  need. This version prints out reads, one per x value."
  [cov-vector ref-name ref-len outdir id]
  (let [outd (clojure.string/replace outdir #"/$" "")
        outf (format "%s/tmp.2394230498397.r" outd)
        freqs (frequencies (flatten cov-vector))
        xs (range 1 (inc ref-len))
        ;; fill ys with zeros for areas of no coverage
        ys (map (fn [x] (if (contains? freqs x) (freqs x) 0)) xs)
        [x y] (map #(format "c(%s)" %) 
                   [(clojure.string/join ", " xs) 
                    (clojure.string/join ", " ys)])
        xy-strings (get-xy-strings cov-vector)
        points (clojure.string/join "\n" 
                                    (map #(format "points(%s, type='l', lwd=2, col='green')" %) 
                                         xy-strings))]
    (spit outf
          (format (str (format "pdf('%s/%s_cov_%s.pdf', width=8, height=5)\n" 
                            outd ref-name id ref-name id)
                    "plot(x=%s, y=%s, main='%s %s', xlab='Position', ylab='Coverage', "
                    "type='l', lwd=3, ylim=c(0, %s))\n"
                    points
                    "\ninvisible(dev.off())\n") 
               x y ref-name id (count cov-vector)))
    (r-script outf)))

(defn cov-vec [read-info-map]
  (range (:start read-info-map) (inc (:end read-info-map))))

(defn plot-cov-for-info-map 
  "INPUT: {:seq1 [{...read info maps...} {} {}]

   type is either 'mapped_reads' or 'mapped_proper_fragments'"
  [info ref-lengths outdir type]
  (map (fn [[ref info-maps]] 
           (plot-cov (map cov-vec info-maps)
                     (name ref)
                     (ref ref-lengths)
                     outdir
                     type)) 
         info))
