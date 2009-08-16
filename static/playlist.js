var artistTemplate;
var albumTemplate;
var songTemplate;
var entryTemplate;

var playlist_albums = {};
var playlist_album_songs = {};
var playlist_entries = {};
var entry_id_counter = 0;

var playlist_setup = function() {
    artistTemplate = $("#folderTemplate").removeAttr("id").remove();
    albumTemplate = $("#albumTemplate").removeAttr("id").remove();
    songTemplate = $("#songTemplate").removeAttr("id").remove();
    entryTemplate = $("#entryTemplate").removeAttr("id").remove();

    playlist_reload_library();
    
    $("#playlist").sortable({
        receive: playlist_entry_added,
    });
};

var playlist_reload_library = function() {
    playlist_albums = {};
    playlist_album_songs = {};
    var library = $("#library");    
    library.html("Loading...");
    $.getJSON("/playlist/summary/artists", {},
              function(json) {                  
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
        subBox.draggable({appendTo: 'body', 
                          containment: 'document', 
                          helper: function() { return playlist_make_entry(song); },
                          connectToSortable: '#playlist'});

        subBox.find(".editButton").click(function() { playlist_edit_song(subBox, song); });

        songs.push([song['track'], subBox]);
    });

    songs.sort(function(a, b) { return a[0] - b[0]});
    
    var songBox = $("<div>").empty();
    $.each(songs, function(i, s) { songBox.append(s[1]); });

    playlist_album_songs[artist][album] = songBox;
};


var playlist_make_entry = function(song) {
    ph = entryTemplate.clone().populate(song);
    ph.attr("json", JSON.stringify(song));
    return ph.get(0);
};

var playlist_edit_song = function(subBox, song) {
    var popup = $("#songEdit");
    var offset = subBox.offset();
    popup.css("top", offset.top + 5);
    popup.css("left", offset.left + 5);

    popup.find("[name='artist']").val(song['artist']);
    popup.find("[name='album']").val(song['album']);
    popup.find("[name='title']").val(song['title']);
    popup.find("[name='track']").val(song['track']);

    popup.find("[type='text']").unbind("keydown").keydown(function(e) { 
        if (e.keyCode == 13) playlist_confirm_edit(song); 
    });

    popup.find("[value='Cancel']").unbind("click").click(function() { playlist_cancel_edit(); })
    popup.find("[value='Save']").unbind("click").click(function() { playlist_confirm_edit(song); })

    popup.css("display", "block");
};

var playlist_cancel_edit = function() {
    $("#songEdit").css("display", "none");
};

var playlist_confirm_edit = function(song) {
    var popup = $("#songEdit");
    var json = {
        'file': song['file'],
        'artist': popup.find("[name='artist']").val(),
        'album': popup.find("[name='album']").val(),
        'title': popup.find("[name='title']").val(),
        'track': popup.find("[name='track']").val()
    };
    $.post("/playlist/edit", json, playlist_edit_complete, "json");
};

var playlist_edit_complete = function(json) { 
    $("#songEdit").css("display", "none");
    playlist_reload_library();
};



var playlist_entry_added = function(e, ui) {

    var entryID = "entry_ " + entry_id_counter++;

    var helper = ui.helper.clone();
    helper.removeAttr("left").removeAttr("top").removeAttr("position");

    var json = JSON.parse(helper.attr("json"));

    playlist_entries[entryID] = json;

    var entry = $("#playlist")
        .find(".song")
        .attr("class", "entry ui-draggable")
        .attr("id", entryID)
        .html(helper.html());

    var remover = $('<img src="/static/close.png" style="float: right;" />')
        .click(function() { 
            entry.remove();
            delete playlist_entries[entryID];
        });

    entry.prepend(remover);
}