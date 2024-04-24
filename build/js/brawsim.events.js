module.exports =  {

    makeValues_changed: function(ui) {
    let value = ui.makeValues.value();
    if (value) 
        ui.sendValues.setValue(!ui.makeValues.value());
    }
};
