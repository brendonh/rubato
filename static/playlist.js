var artistTemplate;
var albumTemplate;

var playlist_setup = function() {
    artistTemplate = $("#folderTemplate").removeAttr("id").remove();
    albumTemplate = $("#albumTemplate").removeAttr("id").remove();

    $.getJSON("/playlist/summary/artists", {},
              function(json) {
                  var library = $("#library");
                  library.html("");
                  var boxes = [];
                  $.each(json, function(artist, count) {
                      var box = artistTemplate.clone().populate({'artist': artist, 'total': count});
                      box.click(function() { playlist_expand_artist(artist, box); });
                      boxes.push([artist, box]);
                  });
                  boxes.sort();
                  $.each(boxes, function(i, stuff) {
                      stuff[1].css('display', 'block').appendTo(library);
                  });
              });
};

var playlist_albums = {};

var playlist_expand_artist = function(artist, box) {

    if (!playlist_albums[artist]) {
        box.unbind("click");
        $.getJSON("/playlist/summary/albums", {artist: artist},
                  function(json) { 
                      playlist_insert_albums(artist, json);
                      playlist_expand_artist(artist, box);
                      box.click(function() { playlist_expand_artist(artist, box); });
                  });
        return;
    }

    if (box.attr("expanded")) {
        playlist_albums[artist].remove();
        box.removeAttr("expanded");
        return;
    }

    box.after(playlist_albums[artist]);
    box.attr("expanded", "1");
};



var playlist_insert_albums = function(artist, json) {     
    var albums = new Array();
    $.each(json, function(album, count) {
        var subBox = albumTemplate.clone().populate({'album': album, 'total': count});
        subBox.css('display', 'block');
        albums.push([album, subBox]);
    });
    
    albums.sort();
    
    var albumBox = $("<div>").empty();
    $.each(albums, function(i, a) { albumBox.append(a[1]); });
    playlist_albums[artist] = albumBox;
};