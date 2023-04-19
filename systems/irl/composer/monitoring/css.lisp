
(in-package :irl)

;; ------------------------
;; + CSS for web monitors +
;; ------------------------

(define-css 'cer "
div.cer {
	display: inline-block;
	margin-right: 0px;
	margin-top: 10px;
	margin-bottom: 10px;
	padding: 0px;
}

div.cer-box {
	border: 1px solid #562;
	display: inline-block;
}

div.cer div.cer-title {
	padding: 0px;
	padding-left: 3px;
	padding-right: 3px;
	white-space: nowrap;
	background-color: #562;
}

div.cer div.cer-title>a {
	color: #fff;
}

div.cer div.cer-title>span {
	color: #fff;
}

div.cer-float {
	display: inline-block;
	margin-right: 10px;
	margin-top: -6px;
	margin-bottom: 8px;
}

table.cer {
	border-collapse: collapse;
}

table.cer td.cer-type {
	font-style: italic;
	padding: 0px;
	padding-left: 4px;
}

table.cer td.cer-details {
	vertical-align: top;
	padding-top: 3px;
	padding-bottom: 3px;
	padding-left: 5px;
	padding-right: 5px;
}

table.cer td.cer-details div.cer-detail {
	padding-left: 4px;
	padding-right: 4px;
	padding-bottom: 1px;
	padding-top: 1px;
	border-top: 1px dashed #563;
	text-align: left;
}

table.cer td.cer-details>div {
	overflow: hidden;
}

table.cer td.cer-details div.cer-detail:first-child {
	border-top: none;
}

div.cer-hidden-subtree {
	padding: 0px;
	margin: 0px;
	padding: 0px;
	margin-bottom: 2px;
}
")

(define-css 'ccn "
div.ccn {
  display: inline-block;
  margin-right: 0px;
  margin-top: 10px;
  margin-bottom: 10px;
  padding: 0px;
}

div.ccn-box {
  border: 1px solid #562;
  display: inline-block;
}

div.ccn div.ccn-title {
  padding: 0px;
  padding-left: 3px;
  padding-right: 3px;
  white-space: nowrap;
  background-color: #562;
}

div.ccn div.ccn-title>a {
  color: #fff;
}

div.ccn div.ccn-title>span {
  color: #fff;
}

div.ccn-float {
  display: inline-block;
  margin-right: 10px;
  margin-top: -6px;
  margin-bottom: 8px;
}

table.ccn {
  border-collapse: collapse;
}

table.ccn td.ccn-type {
  font-style: italic;
  padding: 0px;
  padding-left: 4px;
}

table.ccn td.ccn-details {
  vertical-align: top;
  padding-top: 3px;
  padding-bottom: 3px;
  padding-left: 5px;
  padding-right: 5px;
}

table.ccn td.ccn-details div.ccn-detail {
  padding-left: 4px;
  padding-right: 4px;
  padding-bottom: 1px;
  padding-top: 1px;
  border-top: 1px dashed #563;
  text-align: left;
}

table.ccn td.ccn-details>div {
  overflow: hidden;
}

table.ccn td.ccn-details div.ccn-detail:first-child {
  border-top: none;
}

div.ccn-hidden-subtree {
  padding: 0px;
  margin: 0px;
  padding: 0px;
  margin-bottom: 2px;
}
")

(define-css 'chunk-composer "
div.chunk-composer {
	display: inline-block;
	margin-right: 10px;
	margin-top: 4px;
	margin-bottom: 4px;
	padding: 0px;
}

div.chunk-composer-box {
	border: 1px solid;
	display: inline-block;
}

div.chunk-composer div.chunk-composer-title {
	padding: 0px;
	padding-left: 3px;
	padding-right: 3px;
	white-space: nowrap;
}

div.chunk-composer div.chunk-composer-title>a {
	color: #40241A;
	font-weight: bold;
}

table.chunk-composer {
	border-collapse: collapse;
}

table.chunk-composer td.chunk-composer-type {
	font-style: italic;
	padding: 0px;
	padding-left: 4px;
}

table.chunk-composer td.chunk-composer-details div.chunk-composer-detail {
	padding-left: 4px;
	padding-right: 4px;
	padding-bottom: 1px;
	padding-top: 1px;
	border-top: 1px dashed #563;
	text-align: left;
}

table.chunk-composer td.chunk-composer-details>div {
	overflow: hidden;
}

table.chunk-composer td.chunk-composer-details div.chunk-composer-detail:first-child {
	border-top: none;
}
")

(define-css 'indent-irpf "div.indent-irpf { margin-left: 45px; }")