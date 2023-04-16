
# Table of Contents

1.  [fd-find](#orgc4ccfb2)
    1.  [intro](#orgeb7b309)
    2.  [documentation](#org3a0aa46)
    3.  [roadmap](#orgc73723f)



<a id="orgc4ccfb2"></a>

# fd-find


<a id="orgeb7b309"></a>

## intro

My first package for emacs to leverage the search command fd in an emacs buffer.


<a id="org3a0aa46"></a>

## documentation

To use this package, save the above code to a file called \`fd-find.el\` in your Emacs load path (e.g. \`~/.emacs.d/lisp/\`) and add the following line to your Emacs init file:

\`\`\`emacs-lisp
(require 'fd-find)
\`\`\`

Then you can use the command \`fd-find\` (bound to \`M-x fd-find\`) to search for files in the current directory and its subdirectories using \`fd\`, and display the results in a buffer called \`\*fd-find\*\`. The search query is a string that can contain name patterns, file extensions, or regular expressions. You can enter multiple patterns separated by spaces, and use quotes to group them. For example:

-   \`fd-find\` to find all files in the current directory
-   \`fd-find jpg\` to find all JPG files in the current directory
-   \`fd-find "image/\*.jpg"\` to find all JPG files in the \`image\` subdirectory
-   \`fd-find '.\*\\.el$'\` to find all Emacs Lisp files in the current directory and its subdirectories
-   \`fd-find foo bar\` to find all files that contain the words "foo" or "bar" in the current directory and its subdirectories.

If you want to sort the results by file size, you can pass a
  prefix argument to the command, \`C-u M-x fd-find\`. This will
  sort the files in decreasing order of size, starting with the
  largest one.

To open a file, move the point to the corresponding line and press \`RET\`. This will open the file in another buffer. You can also click on the link with the mouse to open the file.


<a id="orgc73723f"></a>

## roadmap
