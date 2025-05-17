function escapeHTML(str) {
    return str
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#039;');
}

function copyToClipboard(text) {
    if (navigator.clipboard && window.isSecureContext) {
        return navigator.clipboard.writeText(text);
    } else {
        let textArea = document.createElement("textarea");
        textArea.value = text;
        textArea.style.position = "absolute"; // Prevent scrolling to bottom of page in MS Edge.
        textArea.style.opacity = "0"; // Make it invisible
        textArea.style.left = "-9999px"; // Move it off-screen
        textArea.style.top = "-9999px"; // Reset top position
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        return new Promise((res, rej) => {
            document.execCommand('copy') ? res() : rej();
            textArea.remove();
        });
    }
}

window.onload = async function() {
    const form = document.getElementById('upload-form');
    form.addEventListener('submit', async function(event) {
        event.preventDefault(); // Prevent the default form submission

        const formData = new FormData(form);
        try {
            const res = await fetch('/upload', {
                method: 'POST',
                body: formData
            });
            const rt = document.getElementById('upload-result');
            if (res.ok) {
                let resText = await res.text();
                rt.innerHTML = '文件上传成功，将内容粘贴至文本中 : ' + escapeHTML(resText);
                copyToClipboard(resText).then(() => {
                    rt.innerHTML = '文件上传成功，内容已复制到剪贴板';
                }).catch(() => {
                    rt.innerHTML = '文件上传成功，请手动复制以下红色内容至文章正文： <span color=red>' + escapeHTML(resText) + '</span>';
                });
            } else {
                let resText = await res.text();
                rt.innerHTML = '文件上传失败 : ' + resText;
            }
        } catch (error) {
            console.error('Error uploading file:', error);
            window.alert('Error uploading file: ' + error);
        }
    });
}