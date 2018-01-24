
;; smex is handled by the `ivy' layer and we don't want
;; to use the ownership mechanism of layers because it is dependent
;; on the order of layer declaration
(configuration-layer/remove-layer 'smex)
