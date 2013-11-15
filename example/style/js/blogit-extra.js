
// Color Bash prompt in example block
function colorShellPrompt () {
    var reSearch = /(\w*@\w*)\s*([:](.+)\/([^/]+)[#$])/;
    var reReplace = '<font color=\"lightgreen\">$1</font><font color=\"lightblue\">$2</font>';
    var block = document.getElementsByClassName('example');
    for(var i = 0, l = block.length; i < l; i++) {
        block[i].innerHTML = block[i].innerHTML.replace(reSearch, reReplace);
    }
}


$(document).ready(function () {
    colorShellPrompt();
});
