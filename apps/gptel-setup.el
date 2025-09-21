;;; gptel-setup.el --- Gptel settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings of the GPTEL plugins and its respective hotkeys

;;; Code:

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-api-key (my/select-and-set-openai-key))
  (gptel-directives
   '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
   (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
   (writing . "You are a large language model and a writing assistant. Respond concisely.")
   (chat . "You are a large language model and a conversation partner. Respond concisely.")

   (cpp . " This is LLM is used to learn c++. Explain in-depth underlying contecepts of C++, theris implementations in the standard and starndard library.  Build the following sections in the end of your respond - 1. furher reading and invistigation section: articles, conference talks, books. 2. \"Experimentation\" section in which tell how to build small scripts and programs. 3. =pro-tips= and =hacks=. DO NOT REPEAT YOURSELF on those sections, if already have built related sections previously. Adhere to best practicies of the industry."))

   (cpp-linux . "This is LLM is used to learn *C++ LINUX SYSTEM PROGRAMMING* by *EXPLORING LINUX C-API AND LIBRARIES*. Explain in-depth underlying contecepts of C++, theris implementations in the standard and starndard library. Explain which role OS and hardware has in those. Make a special emphasis on Linux Kernal Implementations of the Core OS Principles and Ideas. You should dig into deep linux OS kernel C-code while explaining, yet in your code stick to modern C++ usages. Build the following sections in the end of your respond - 1. furher reading and invistigation section: articles, conference talks, books. 2. \"Experimentation\" section in which tell how to build small scripts and programs. 3. =pro-tips= and =hacks=. DO NOT REPEAT YOURSELF on those sections, if already have built related sections previously. Adhere to best practicies of the industry.")

   (linux . "This is LLM is used to learn linux OS. Explain in-depth underlying contecepts of OSes and theris implementations in Linux. Explore core linux foundations deep into the kernel (in C).")

  ))


(gptel-make-anthropic "CLAUDE" :stream t :key
     '"***REMOVED***")


(with-eval-after-load 'gptel
  (evil-define-key 'normal org-mode-map
    (kbd "M-g G") #'my/gptel-setup-chat
    (kbd "M-g M-g") #'my/gptel-make-prompt
    ))

(provide 'gptel-setup)
;;; gptel-setup.el ends here
