# CL-WIKIMEDIA
[Remi van Trijp](remi.vantrijp@sony.com) version 0.1

A lightweight interface to the Wikimedia APIs. The current version provides hooks for interfacing with the [Wikimedia REST API](https://www.mediawiki.org/wiki/Wikimedia_REST_API), other APIs may follow later. In order to load the library, simply evaluate the following:

	(ql:quickload :cl-wikimedia)


## Wikimedia REST API

This section explains how to interface with the Wikimedia REST API. Please adhere to these global rules as specified by Mediawiki:

* Limit your clients to no more than 200 requests/s to this API. Each API endpoint's documentation may detail more specific usage limits.
* Set a unique User-Agent or Api-User-Agent header that allows us to contact you quickly. Email addresses or URLs of contact pages work well.

You can set your user agent in the file config.lisp as follows (replace with your own e-mail):

	(setf *user-agent* {your.email@domain.com})

### Main function: `wikimedia-rest-api`

The most flexible way to access the API endpoints is to use the function `wikimedia-rest-api`. You can check [this documentation page](https://en.wikipedia.org/api/rest_v1/) for a list of API endpoints.

Obligatory arguments:

* api-endpoint (<i>string</i>): One of the API endpoints as specified in [this documentation page](https://en.wikipedia.org/api/rest_v1/).

Optional keys:

* :user-agent (<i>string</i>): Please specify your :user-agent in the file config.lisp.
* :language (<i>string</i>): One of the languages supported by Wikipedia, such as "en" (default value).
* :method: Either `:get` (default) or `:post`
* :content-type: Set to "application/json" by default.
* :lisp-format: Either `:alist` (default, recommended for inspection purposes), `:plist`, or `:hash-table` (recommended for most uses)

#### Example 1

The following example accesses the API endpoint `page/`, which returns a list of page-related API entry points.

	(wikimedia-rest-api "page/")

#### Example 2: Changing the language

By default, the interface will interface with the English wikipedia, but you can modify this through the keyword `:language`, which you can set to any language code supported by Wikipedia. The following example will therefore look for information about Luc Steels in the English Wikipedia:

	(wikimedia-rest-api "page/title/Luc_Steels") 

This example, on the other hand, searches the French wikipedia:

	(wikimedia-rest-api "page/title/Luc_Steels"
		:language "fr") 

#### Example 3: Changing the returned format

By default, `wikimedia-rest-api` returns the requested information as two values: an Alist, and a hash-table. Your functions can therefore obtain both types of information using `multiple-value-bind`:

	(multiple-value-bind (alist hashtable)
			(wikimedia-rest-api "page/")
		...)

The Alist representation is useful for inspecting the returned results more intuitively. However, for requests that expect big amounts of data, we recommend to change the return values to :hash-table as follows:

	(wikimedia-rest-api "page/"
		:lisp-format :hash-table)

In this case, both returned values are the same hash-table object. The additional advantage of the hash-table is that it is straightforward to access information because you can use the json properties of the returned information as hash keys:

	(gethash "items"
		(wikimedia-rest-api "page/"
			:lisp-format :hash-table))

#### Other keys

In most cases, you won't need to touch the other keys, except for changing the method of the http-request from `:get` to `:post` using the `:method` key. 

With `wikimedia-rest-api`, you can therefore access all information following the [Wikimedia REST API](https://en.wikipedia.org/api/rest_v1/). The following subsections however offer some useful helper functions that make it easier to access certain information that is useful for building NLP applications so you do not have to be an expert in the Wikimedia REST API.

### Obtaining Summaries

<b>Status: Currently assumes that you enter page titles that can be found. Error handling is in development.</b>

#### `wikimedia-summary`

Returns several properties about a requested page, such as its title, description, an extract (the actual summary) as plain text or html, an image (including its properties and its thumbnail), and so on.

Obligatory Arguments:

* title <i>(string)</i>: The title of a page. Any whitespace will be replaced by underscores to fit the canonical title format of Wikimedia (e.g. "Luc_Steels" and "Fluid_construction_grammar"). Page titles are case sensitive.

Optional keys:

* :language <i>(string)</i>: Default value is "en". Can be replaced by other language codes supported by Wikipedia, such as "fr" (French).
* :lisp-format <i>(string)</i>: Default value is `:hash-table`. Other options are `:alist` and `:plist`.

We recommend to keep the return format to `:hash-table` because all of the helper functions for accessing

##### Example
	`(wikimedia-summary "Fluid construction grammar")

#### Accessing data

If you are familiar with the Wikimedia REST API, you can simply use json properties as hashkeys for accessing data from a summary. For example:

	(gethash "description" (wikimedia-summary "Sony"))
	> "Japanese multinational conglomorate corporation"
	T

You can also use your Lisp environment's autocomplete function as a reminder to use the helper functions for accessing these data by typing `wikimedia-summary-` and then hitting your autocompletion button. Here is an example of a helper function:

	(wikimedia-summary-description 
		(wikimedia-summary "Sony"))
	> "Japanese multinational conglomorate corporation"
	T

Since these helper functions are really just calling `gethash` underneath, they always return to functions: the hashed value (as obtained by using a key), and a second value that states whether the key existed (T or NIL. For example, for the page "Fluid_construction_grammar", no description is provided at the moment of writing this documentation:

	(wikimedia-summary-description 
		(wikimedia-summary "Fluid construction grammar"))
	> NIL
	NIL

While in most cases it is safe to assume that NIL as the first value means that there is no entry for this particular property, it is always safer to check for the second value whether the key existed or not.