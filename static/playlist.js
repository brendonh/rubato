var playlist_setup = function() {
    $.getJSON("/playlist/summary/artists", {},
              function(json) {
                  var template = $("#folderTemplate");
                  var library = $("#library");
                  library.html("");
                  var boxes = [];
                  $.each(json, function(artist, count) {
                      var box = template.clone().populate({'artist': artist, 'total': count});
                      boxes.push([artist, box]);
                  });
                  boxes.sort();
                  $.each(boxes, function(i, stuff) {
                      stuff[1].contents().appendTo(library);
                  });
              });
};