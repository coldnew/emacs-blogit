
// Color Bash prompt in example block
function colorShellPrompt () {

    // prevent selectable in shell prompt
    function span (x) {
//	var pre = '<span onmousedown=\"return false;\" onselectstart=\"return false;\">';
//	var pos = '</span>';
	var pre = "";
	var pos = "";
	return pre + x + pos;
    }

    var userHighlight  = span ('<font color=\"lightgreen\">$1</font><font color=\"lightblue\">$2</font>');
    var rootHighlight  = span ('<font color=\"crimson\">$1</font><font color=\"lightblue\">$2</font>');
    var block = document.getElementsByClassName('example');
    for(var i = 0, l = block.length; i < l; i++) {
        // highlight `user@hostname directory $'
        block[i].innerHTML = block[i].innerHTML.replace(/^(\w*@\w*)(\s*[:~](.+)\/([^/]+)[$])/, userHighlight);
        // highlight `user@hostname ~ $'
        block[i].innerHTML = block[i].innerHTML.replace(/^(\w*@\w*)(\s*[:~](.*)([^/]+)[$])/, userHighlight);
        // highlight `root@hostname #'
        block[i].innerHTML = block[i].innerHTML.replace(/^(root@\w*)(\s*[:~](.+)\/([^/]+)[#])/, rootHighlight);
        // highlight `hostname #'
        block[i].innerHTML = block[i].innerHTML.replace(/^(\w*)(\s*[:~](.+)\/([^/]+)[#])/, rootHighlight);
        // highlight `hostname directory #' (Gentoo Linux root)
        block[i].innerHTML = block[i].innerHTML.replace(/^(\w*)(\s*\w* [#])/, rootHighlight);
    }
}

$(document).ready(function () {
    colorShellPrompt();
});
