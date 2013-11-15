
// Color Bash prompt in example block
function colorShellPrompt () {
    var userHighlight  = '<font color=\"lightgreen\">$1</font><font color=\"lightblue\">$2</font>';
    var rootHighlight  = '<font color=\"crimson\">$1</font><font color=\"lightblue\">$2</font>';
    var block = document.getElementsByClassName('example');
    for(var i = 0, l = block.length; i < l; i++) {
        // highlight `user@hostname $'
        block[i].innerHTML = block[i].innerHTML.replace(/^(\w*@\w*)\s*([:](.+)\/([^/]+)[$])/, userHighlight);
        // highlight `root@hostname #'
        block[i].innerHTML = block[i].innerHTML.replace(/^(root@\w*)\s*([:](.+)\/([^/]+)[#])/, rootHighlight);
        // highlight `hostname #'
        block[i].innerHTML = block[i].innerHTML.replace(/^(\w*)\s*([:](.+)\/([^/]+)[#])/, rootHighlight);
    }
}

$(document).ready(function () {
    colorShellPrompt();
});
