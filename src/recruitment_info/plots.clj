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
  (:use [clojure.java.shell :only [sh]]))

(defn r [r-string]
  (let [{:keys [exit out err]} (apply sh ["Rscript" "-e" r-string])] 
    (if (zero? exit) out err)))

(defn plot-cov
  "cov-vector is a vector like so [[2 3 4] [3 4 5 6] [5 6 7 8
  9]]. outdir will not have trailing '/'"
  [cov-vector ref-name outdir id]
  (let [freqs (sort (frequencies (flatten cov-vector)))
        [x y] (map #(format "c(%s)" %) 
                   [(clojure.string/join ", " (keys freqs)) 
                    (clojure.string/join ", " (vals freqs))])]
    (r (format (str (format "pdf('%s/%s_cov_%s.pdf', width=8, height=5);" 
                            outdir ref-name id)
                    "plot(x=%s, y=%s, main='%s %s', xlab='Position', ylab='Coverage', "
                    "type='l');"
                    "invisible(dev.off());") 
               x y ref-name id))))
