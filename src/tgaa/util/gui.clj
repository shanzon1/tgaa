(ns tgaa.util.gui
  (:import [javax.swing JFileChooser JOptionPane JFrame]
           [javax.swing.filechooser FileNameExtensionFilter FileSystemView]
           [java.io File]))


(defn option-dialog [question ]
  (JOptionPane/showOptionDialog (JFrame.) question "Select an Option."
                                JOptionPane/YES_NO_CANCEL_OPTION JOptionPane/QUESTION_MESSAGE
                                nil nil nil))


(defn sys-view-ref
  ([title]
    (sys-view-ref title :folder nil))
  ([title type-key]
    (sys-view-ref title type-key nil))
  ([title type-key dir]
    (let [select-type (if (= type-key :folder) 1 0)
          jfc (new JFileChooser)
          _ (. jfc setDialogTitle title)
          crn-dir (if (nil? dir) 
                    (. (. javax.swing.filechooser.FileSystemView getFileSystemView) getHomeDirectory)
                    (new File dir))
          _ (.  jfc setFileSelectionMode select-type)
          _  (. jfc setCurrentDirectory crn-dir) 
          retval (.showOpenDialog jfc nil)]
      (if (= retval JFileChooser/APPROVE_OPTION)
        (.getSelectedFile jfc)
        nil))))


(defn sys-folder-all-files [title]
  (vec (file-seq 
         (sys-view-ref title))))
