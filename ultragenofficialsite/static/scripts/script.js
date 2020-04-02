function convertMdToHtml (obj) {
    const converter = new showdown.Converter();
    const md = obj.innerText;
    const html = converter.makeHtml(md);
    return html;
}
//var mainText = document.getElementById('md_text');
var toMD = document.getElementsByClassName('md_text');
for(i = 0; i < toMD.length; i++) {    
    toMD[i].innerHTML = convertMdToHtml(toMD[i]);
}