(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defun boris-widget-example (tags)
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (dolist (tag (boris-tags-hierarchy tags))
    (apply 'widget-create
           (boris-tag-to-widget tag)))
  (use-local-map widget-keymap)
  (widget-setup))

(defun boris-tags-hierarchy (tags)
  (let ((child-index (make-hash-table :test 'equal)))
    (dolist (tag tags)
      (let ((parent-name (plist-get tag :parent)))
        (push tag (gethash parent-name child-index))))
    
    (cl-labels ((tag-with-children (tag)
                  (let* ((tag-name (plist-get tag :name))
                         (children
                          (mapcar #'tag-with-children
                                  (gethash tag-name child-index))))
                    (plist-put tag :children children))))
      (mapcar #'tag-with-children
              (gethash nil child-index)))))

(defun boris-tag-to-widget (tag)
  (cl-destructuring-bind (&key name children description &allow-other-keys)
      tag
    `(tree-widget
      :tag ,name
      :open t
      ,@(mapcar #'boris-tag-to-widget children))))

(boris-widget-example
 (boris-call (list :operation :whoextends
                   :class "views_object")))
