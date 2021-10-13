//---------Flags-----------
var promidat_flat_models = {knn: true, dt: true, rf: true, 
                            boosting: true, svm: true, rl: true, 
                            nn: true, rlr: true, rd: true};
                            
generating_model = false
//-------------------------

function promidat_model_firt(e, model, id){
  if(promidat_flat_models[model]){
    promidat_flat_models[model] = false;
    $("#"+id).click();
  }
}

 window.addEventListener("load", function(event) {
  /* Al cargarse la página  se pone el titulo */
  $("header").find("nav").append('<span class="header-title"> <i>regresso</i>R </span>');
  
  //eliminar_tabs_extras()

  /* Los modelos se vuelven ejecutar al ser seleccionados una vez se haga una segmentacion diferente*/
  $("a[href^='#shiny-tab-knn']").on('click', function(e){promidat_model_firt(e,"knn","KNN_ui_1-runKnn")});
  $("a[href^='#shiny-tab-dt']").on('click', function(e){promidat_model_firt(e,"dt","regression_trees_ui_1-runDt")});
  $("a[href^='#shiny-tab-rf']").on('click', function(e){promidat_model_firt(e,"rf","random_forests_ui_1-runRf")});
  $("a[href^='#shiny-tab-boosting']").on('click', function(e){promidat_model_firt(e,"boosting","boosting_ui_1-runBoosting")});
  $("a[href^='#shiny-tab-svm']").on('click', function(e){promidat_model_firt(e,"svm","SVM_ui_1-runSvm")});
  $("a[href^='#shiny-tab-nn']").on('click', function(e){promidat_model_firt(e,"nn","neural_networks_ui_1-runNn")});
  $("a[href^='#shiny-tab-rl']").on('click', function(e){promidat_model_firt(e,"rl","linear_regression_ui_1-runRl")});
  $("a[href^='#shiny-tab-rlr']").on('click', function(e){promidat_model_firt(e,"rlr","penalized_Regression_ui_1-runRlr")});
  $("a[href^='#shiny-tab-rd']").on('click', function(e){promidat_model_firt(e,"rd","dimension_reduction_ui_1-runRd")});
  $("#carga_datos_ui_1-segmentButton").on('click',function(e){
    promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true, rl: true, nn: true, rlr: true, rd:true};
  });
});

/*
function eliminar_tabs_extras(){
  $("ul#normal_ui_1-BoxNormal li")[3].remove();
  $("ul#dispersion_ui_1-BoxDisp li")[1].remove();
  $("ul#distribuciones_ui_1-tabDyA li")[2].remove();
  $("ul#distribuciones_ui_1-tabDyA li")[2].remove();
  $("ul#correlacion_ui_1-tabCor li").last().remove();
  $("ul#correlacion_ui_1-tabCor li").last().remove();
  $("ul#Predictive_Power_ui_1-BoxPodPred li").last().remove();
  
  $("ul#linear_regression_ui_1-BoxRl li").last().remove();
  $("ul#penalized_Regression_ui_1-BoxRlr li").last().remove();
  $("ul#penalized_Regression_ui_1-BoxRlr li").last().remove();
  $("ul#regression_trees_ui_1-BoxDt li").last().remove();
  $("ul#regression_trees_ui_1-BoxDt li").last().remove();
  $("ul#random_forests_ui_1-BoxRf li").last().remove();
  $("ul#random_forests_ui_1-BoxRf li").last().remove();
  $("ul#boosting_ui_1-BoxB li").last().remove();
  $("ul#boosting_ui_1-BoxB li").last().remove();
  $("ul#KNN_ui_1-BoxKnn li").last().remove();
  $("ul#KNN_ui_1-BoxKnn li").last().remove();
  $("ul#SVM_ui_1-BoxSvm li").last().remove();
  $("ul#SVM_ui_1-BoxSvm li").last().remove();
  $("ul#dimension_reduction_ui_1-BoxRd li").last().remove();
  $("ul#dimension_reduction_ui_1-BoxRd li").last().remove();
  $("ul#neural_networks_ui_1-BoxNn li").last().remove();
  $("ul#neural_networks_ui_1-BoxNn li").last().remove();
  
  $("ul#new_data_predictions_ui_1-BoxModel_New_Data li").last().remove();
  $("ul#new_data_predictions_ui_1-BoxPrediccion_New_Data li").last().remove();
}*/


Shiny.addCustomMessageHandler("updateLabel",
  function(message) {
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]")
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
      if(message.ids[i] == "categorico" || message.ids[i] == "numerico" || message.ids[i] == "disyuntivo"){
        $("option[value='"+message.ids[i]+"']").text(message.values[i])
      }
    }
  }
);