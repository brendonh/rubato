var artistTemplate;
var albumTemplate;
var songTemplate;

var playlist_setup = function() {
    artistTemplate = $("#folderTemplate").removeAttr("id").remove();
    albumTemplate = $("#albumTemplate").removeAttr("id").remove();
    songTemplate = $("#songTemplate").removeAttr("id").remove();

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
                      box.after(playlist_albums[artist]);
                      box.click(function() { playlist_expand_artist(artist, box); });
                  });
        return;
    }

    if (box.attr("expanded")) {
        playlist_albums[artist].hide();
        box.removeAttr("expanded");
        return;
    }

    playlist_albums[artist].show();
    box.attr("expanded", "1");
};



var playlist_insert_albums = function(artist, json) {     
    var albums = new Array();
    $.each(json, function(album, count) {
        var subBox = albumTemplate.clone().populate({'album': album, 'total': count});
        subBox.css('display', 'block');
        subBox.click(function() { playlist_expand_album(artist, album, subBox); });
        albums.push([album, subBox]);
    });
    
    albums.sort();
    
    var albumBox = $("<div>").empty();
    $.each(albums, function(i, a) { albumBox.append(a[1]); });
    playlist_albums[artist] = albumBox;
};

var playlist_album_songs = {};

var playlist_expand_album = function(artist, album, box) {

    if (!playlist_album_songs[artist]) {
        playlist_album_songs[artist] = {};
    }

    if (!playlist_album_songs[artist][album]) {

        box.unbind("click");
        $.getJSON("/playlist/summary/songs", {artist: artist, album: album},
                  function(json) { 
                      playlist_insert_songs(artist, album, json);
                      playlist_expand_album(artist, album, box);
                      box.after(playlist_album_songs[artist][album]);
                      box.click(function() { playlist_expand_album(artist, album, box); });
                  });
        return;
    }

    if (box.attr("expanded")) {
        playlist_album_songs[artist][album].hide();
        box.removeAttr("expanded");
        return;
    }

    playlist_album_songs[artist][album].show();
    box.attr("expanded", "1");

};

var playlist_insert_songs = function(artist, album, json) {

    var songs = new Array();
    $.each(json['songs'], function(i, song) {
        var subBox = songTemplate.clone().populate(song);
        subBox.css('display', 'block');
        songs.push([song['track'], subBox]);
    });

    songs.sort(function(a, b) { return a[0] - b[0]});
    
    var songBox = $("<div>").empty();
    $.each(songs, function(i, s) { songBox.append(s[1]); });

    playlist_album_songs[artist][album] = songBox;
};