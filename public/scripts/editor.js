function rollback(id) {
    let confirm = window.confirm('是否放弃本次编辑？');
    if (confirm) {
        fetch('/delete_empty/' + id);
        window.location.href = '/';
    }
}

function new_subpage(id) {
    let content = document.getElementById('contents');
    form = new FormData();
    form.append('id', id);
    fetch('/editor/new_sub', {
        method: 'POST',
        body: form,
    }).then((res) => {
        if (res.ok) {
            res.text().then((text) => {
                content.value += `\n\n[新子页面](/view_article/${text})\n`
                // let form = document.getElementById('editor-form');

                window.alert("即将跳转新界面");
                let form = document.getElementById('editor-form');
                formData = new FormData(form);
                fetch('/save_article/' + id, {
                    method : 'POST',
                    body: formData,
                }).then((_) => {
                    window.location.href = '/editor/' + text;
                }).catch((err) => {
                    window.alert("Failed to create new subpage with error " + err);
                });
            });   
        } else {
            window.alert("Failed to create new subpage with error " + res.status);
        }
    }).catch((err) => {
        window.alert("Failed to create new subpage with error " + err);
    });
}

function preview(id) {
    let form = document.getElementById('editor-form');
    formData = new FormData(form);
    fetch('/save_article/' + id, {
        method : 'POST',
        body: formData,
    }).then((res) => {
        if (res.ok) {
            let preview = document.getElementById('preview_popover');
            preview.showPopover();
        }    
    }).catch((err) => {
        window.alert("Failed to preview with error " + err);
    });
}