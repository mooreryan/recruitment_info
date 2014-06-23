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

(ns recruitment_info.core
  (:require [recruitment_info.alignment-info :refer :all]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class :main true))

(def usage
  (str "\nUSAGE: \njava -jar recruitment_info-x.y.z.jar -b <bam-file> "
       "-i <index-file>"))

(defn exist? [fname]
  (.exists (clojure.java.io/file fname)))

(def cli-options
  ;; An option with a required argument
  [["-b" "--sorted-bam RECRUITMENT.bam" "A sorted BAM file"
    :validate [exist? "The bam file doesn't exist!"]]
   ["-i" "--bam-index RECRUITMENT.bam.bai" "A BAM index file"
    :validate [exist? "The bam index doesn't exist!"]]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])

(defn usage [options-summary]
  (clojure.string/join 
   \newline 
   ["" ""
    "SYNOPSIS:"
    (str "Gives recruitment info provided a bam sorted on coordinate"
         " and an index. Doesn't check if bam is sorted.")
    ""
    (str "USAGE: \njava -jar recruitment_info-x.y.z.jar "
         "-b <bam-file> -i <index-file>")
    ""
    "Options:"
    options-summary
    ""]))

(defn error-msg [errors]
  (str "\nERROR:\n" (clojure.string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]}
        (parse-opts args cli-options)]
    ;; help and error conditions
    (cond
     (:help options) (exit 0 (usage summary))
     (empty? options) (exit 1 (usage summary))
     (not= (count options) 2) (exit 1 (str (error-msg errors) 
                                           (usage summary)))
     errors (exit 1 (error-msg errors)))
    ;; run program with options
    (println (str "#reference\tref_len\tmapped_reads\t"
                  "mean_mapped_read_cov\t"
                  "proper_fragments\tmean_proper_frag_cov"))
    (println
     (clojure.string/join 
      \newline 
      (alignment-info (:sorted-bam options) (:bam-index options))))))
