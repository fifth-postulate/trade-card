(function(Elm){
    var container = document.getElementById('container');
    var app = Elm.TradeCard.Client.embed(container);

    window.destroyDb = function(){
        var db = new PouchDB('card-events');
        db.destroy().then(function(){
            console.log('Database \'card-events\' is destroyed');
        });
    };
})(Elm);
