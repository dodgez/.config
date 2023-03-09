(use-package! request
  :commands 'request)

(map! :leader :prefix "C"
      "e" #'ask-chatgpt
      "i" #'insert-chatgpt
      "I" #'insert-before-region-chatgpt
      "r" #'replace-chatgpt
      "R" #'replace-region-chatgpt
      "t" #'ask-chatgpt-this)
