(require 'ox-publish)
(require 'assemble)

(setq assemble-outdir "build")

(load-file "./publish.el")

(defun default()
  (publish-site assemble-outdir t))
