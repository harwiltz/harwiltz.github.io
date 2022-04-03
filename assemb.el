(require 'ox-publish)
(require 'assemble)

(setq assemble-outdir "build")

(defun default()
  (let ((org-publish-project-alist
	 `(("org-notes"
	    :base-directory "."
	    :base-extension "org"
	    :publishing-directory ,assemble-outdir
	    :recursive t
	    :publishing-function org-html-publish-to-html
	    :headline-levels 4
	    :auto-preamble t)
	   ("org-static"
	    :base-directory "./assets"
	    :base-extension "css\\|js\\|jpg\\|png\\|pdf\\|gif"
	    :publishing-directory ,(concat assemble-outdir "/assets")
	    :recursive t
	    :publishing-function org-publish-attachment)
	   ("org" :components ("org-notes" "org-static")))))
	(org-publish "org")))
