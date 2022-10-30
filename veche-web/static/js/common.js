function submitPostForm(url) {
    let form =
        $('<form method=POST>')
        .attr('action', url)
        .append($('<input type=hidden name=_token>').attr('value', csrfToken));
    $('body').append(form);
    form.submit();
}

$(() => {
    const tooltipTriggerList =
        document.querySelectorAll('[data-bs-toggle="tooltip"]');
    const tooltipList = (
        [...tooltipTriggerList]
        .map(tooltipTriggerEl => new bootstrap.Tooltip(tooltipTriggerEl))
    );
});
