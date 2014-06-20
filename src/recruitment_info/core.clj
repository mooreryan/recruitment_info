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
    "Gives recruitment info for a bam."
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
    (println (str "#reference\tmapped_reads\tmean_mapped_read_cov\t"
                  "proper_fragments\tmean_proper_frag_cov"))
    (println
     (clojure.string/join 
      \newline 
      (alignment-info (:sorted-bam options) (:bam-index options))))))
