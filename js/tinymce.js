tinymce.init({
  selector: 'textarea.tinymce-text',
  height: 150,
  menubar: false,
  placeholder: "Insert narrative here.",
  // plugins: [
  //   'save image',
  //   'advlist autolink lists link image charmap',// print preview anchor',
  //   'visualblocks code fullscreen',
  //   'table paste'
  // ],
  // toolbar: 'undo redo | styleselect | fontselect fontsizeselect |' +
  // 'bold italic backcolor | alignleft aligncenter ' +
  // 'alignright alignjustify | bullist numlist | image table | help',
  plugins: "advlist autolink lists link image charmap visualblocks code table paste hr save", 
  toolbar: "save | undo redo | formatgroup paragraphgroup insertgroup",

  toolbar_groups: {
    formatgroup: {
      icon: 'format',
      tooltip: 'Formatting',
      items: 'fontselect fontsizeselect | bold italic underline strikethrough | forecolor backcolor | superscript subscript | removeformat'
    },
    paragraphgroup: {
      icon: 'paragraph',
      tooltip: 'Paragraph format',
      items: 'styleselect | bullist numlist | alignleft aligncenter alignright alignjustify | indent outdent'
    },
    insertgroup: {
      icon: 'plus',
      tooltip: 'Insert',
      items: 'image table link charmap hr'
    }
  },

  save_onsavecallback: function () {
    localStorage.setItem(tinymce.activeEditor.id, tinymce.activeEditor.getContent())
  },

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

  setup: function (editor) {
    // retrieve stored text from localStorage
    editor.on('init', function () {
      editor.setContent(localStorage.getItem(editor.id));
    });
  },

  content_style: 'body { font-family:Helvetica,Arial,sans-serif; font-size:16px }',
  toolbar_location: 'bottom'
});