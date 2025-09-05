(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-api-key (my/select-and-set-openai-key))
  )

(gptel-make-anthropic :name "CLAUDE" :stream t :key
                      '"***REMOVED***")
