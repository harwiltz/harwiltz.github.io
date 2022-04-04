(require 'ox-publish)
(require 'assemble)

(setq assemble-outdir "build")

(defun default()
  (let ((org-confirm-babel-evaluate nil)
	(org-project-root default-directory)
	(image-file-name-extensions (cons "webm" image-file-name-extensions))
	(org-publish-project-alist
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
	    :base-extension "css\\|js\\|jpg\\|png\\|pdf\\|gif\\|webm"
	    :publishing-directory ,(concat assemble-outdir "/assets")
	    :recursive t
	    :publishing-function org-publish-attachment)
	   ("org" :components ("org-notes" "org-static")))))
	(org-publish "org")))
