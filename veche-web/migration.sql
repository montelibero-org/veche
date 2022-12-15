update comment set type = 'tombstone', message = '' where author = _ and type = 'text';
delete from telegram where id = _;
