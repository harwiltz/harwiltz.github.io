(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(package-install 'htmlize)
(require 'ox-publish)
(require 'ox-html)

(defun publish-site(dir &optional force)
  (load-theme 'tsdh-light)
  (let ((org-confirm-babel-evaluate nil)
		(org-html-validation-link nil)
	(org-project-root default-directory)
	(org-html-htmlize-font-prefix "org-")
	(image-file-name-extensions (cons "webm" image-file-name-extensions))
	(org-publish-project-alist
	 `(("org-notes"
	    :base-directory "."
	    :base-extension "org"
	    :publishing-directory ,dir
	    :recursive t
	    :publishing-function org-html-publish-to-html
	    :headline-levels 4
	    :auto-preamble t)
	   ("org-static"
	    :base-directory "./assets"
	    :base-extension "css\\|js\\|jpg\\|png\\|pdf\\|gif\\|webm\\|mp4\\|webp"
	    :publishing-directory ,(concat dir "/assets")
	    :recursive t
	    :publishing-function org-publish-attachment)
	   ("org" :components ("org-notes" "org-static")))))
    (org-publish "org" force)))
