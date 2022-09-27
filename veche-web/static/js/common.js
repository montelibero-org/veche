function submitPostForm(url) {
    let form =
        $('<form method=POST>')
        .attr('action', url)
        .append($('<input type=hidden name=_token>').attr('value', csrfToken));
    $('body').append(form);
    form.submit();
}

$(() => {
    $('button').each((_, button) => {
        button = $(button);
        const post = button.attr('post');
        if (post) {
            button.click(() => submitPostForm(post));
        }
    });
});
