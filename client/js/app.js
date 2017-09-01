(function(Elm){
    var container = document.getElementById('container');
    var app = Elm.TradeCard.Client.embed(container, {
        user: 'dvberkel',
        lowestCard: 1,
        highestCard: 15
    });

    window.destroyDb = function(){
        var db = new PouchDB('card-events');
        db.destroy().then(function(){
            console.log('Database \'card-events\' is destroyed');
        });
    };
})(Elm);
