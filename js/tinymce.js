tinymce.init({
  selector: 'textarea.tinymce-text',
  height: 150,
  menubar: false,
  placeholder: "Insert narrative here.",
  skin: "outside",
  plugins: [
    'save image',
    'advlist autolink lists link image charmap print preview anchor',
    'searchreplace visualblocks code fullscreen',
    'insertdatetime media table paste code help wordcount'
  ],
  toolbar: 'save | undo redo | styleselect | fontselect fontsizeselect |' +
  'bold italic backcolor | alignleft aligncenter ' +
  'alignright alignjustify | bullist numlist | image table | help',
  /*
    URL of our upload handler (for more details check: https://www.tiny.cloud/docs/configure/file-image-upload/#images_upload_url)
    images_upload_url: 'postAcceptor.php',
    here we add custom filepicker only to Image dialog
  */
  file_picker_types: 'image',
  /* and here's our custom image picker*/
  file_picker_callback: function (cb, value, meta) {
    var input = document.createElement('input');
    input.setAttribute('type', 'file');
    input.setAttribute('accept', 'image/*');

    /*
      Note: In modern browsers input[type="file"] is functional without
      even adding it to the DOM, but that might not be the case in some older
      or quirky browsers like IE, so you might want to add it to the DOM
      just in case, and visually hide it. And do not forget do remove it
      once you do not need it anymore.
    */

    input.onchange = function () {
      var file = this.files[0];

      var reader = new FileReader();
      reader.onload = function () {
        /*
          Note: Now we need to register the blob in TinyMCEs image blob
          registry. In the next release this part hopefully won't be
          necessary, as we are looking to handle it internally.
        */
        var id = 'blobid' + (new Date()).getTime();
        var blobCache =  tinymce.activeEditor.editorUpload.blobCache;
        var base64 = reader.result.split(',')[1];
        var blobInfo = blobCache.create(id, file, base64);
        blobCache.add(blobInfo);

        /* call the callback and populate the Title field with the file name */
        cb(blobInfo.blobUri(), { title: file.name });
      };
      reader.readAsDataURL(file);
    };

    input.click();
  },

  //TODO: Save button call back function
  // save_onsavecallback: function () {  
  //   var FileSaver = require('file-saver');
  //   var content = tinymce.activeEditor.getContent();
  //   // tinymce.activeEditor.execCommand('mceSave');
  //   var blob = new Blob([content], { type: "text/plain;charset=utf-8" });
  //   // downloadToFile(textArea.value, 'my-new-file.txt', 'text/plain');
  //   FileSaver.saveAs(blob, "/Users/Tiffany/test.txt");
  // },
  content_style: 'body { font-family:Helvetica,Arial,sans-serif; font-size:16px }'
});