;;;###autoload
(defun use-chatgpt (messages callback)
  (request "https://api.openai.com/v1/chat/completions"
    :sync t
    :type "POST"
    :data (json-encode `(("messages" . ,messages) ("model" . "gpt-3.5-turbo")))
    :headers `(("Content-Type" . "application/json") ("Authorization" . ,(concat "Bearer " OPENAI_API_KEY)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (s-trim (alist-get 'content (alist-get 'message (aref (alist-get 'choices data) 0)))))))
    :error (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "Got error %S with rest %S" error-thrown args))))
  nil)

;;;###autoload
(defun use-chatgpt-message (single-message callback)
  (use-chatgpt `((("role" . "user") ("content" . ,single-message))) callback))

;;;###autoload
(defun ask-chatgpt (arg)
  (interactive (list (read-from-minibuffer "Prompt for ChatGPT: ")))
  (use-chatgpt-message arg #'message))

;;;###autoload
(defun ask-chatgpt-this ()
  (interactive)
  (when (region-active-p)
    (use-chatgpt-message (buffer-substring (region-beginning) (region-end)) #'message)))

;;;###autoload
(defun insert-chatgpt (arg)
  (interactive (list (read-from-minibuffer "Prompt for ChatGPT: ")))
  (use-chatgpt-message arg #'insert))

;;;###autoload
(defun insert-before-region-chatgpt ()
  (interactive)
  (when (region-active-p)
    (use-chatgpt-message (buffer-substring (region-beginning) (region-end)) #'insert)))

;;;###autoload
(defun replace-chatgpt (arg)
  (interactive (list (read-from-minibuffer "Prompt for ChatGPT: ")))
  (use-chatgpt-message arg #'(lambda (data)
                               (when (region-active-p)
                                 (delete-active-region))
                               (insert data))))

;;;###autoload
(defun replace-region-chatgpt ()
  (interactive)
  (when (region-active-p)
    (use-chatgpt-message
     (buffer-substring (region-beginning) (region-end))
     #'(lambda (data)
         (when (region-active-p)
           (delete-active-region))
         (insert data)))))
