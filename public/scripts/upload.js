function escapeHTML(str) {
    return str
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#039;');
}

window.onload = async function() {
    window.alert('Page loaded');
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
                await navigator.clipboard.writeText(resText);
                rt.innerHTML = '文件上传成功，将内容粘贴至文本中 : ' + escapeHTML(resText);
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