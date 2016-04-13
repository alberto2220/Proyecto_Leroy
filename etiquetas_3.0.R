##################################################
#     SCRIPT PARA REETIQUETADO DE LA COMUNIDAD   #
##################################################

# Una Auditoría previa de como están estructuradas las páginas del site nos obliga a separar
# a separar entre los textos de la categoría de Foros que del resto de categorías.

# RESUMEN DEL PROCEDIMIENTO:
#
# Paso 1.- Carga de las URL's desde el sitemap se CLM.
# Raso 2.- Lectura del html y normalización del texto
# Paso 3.- Comparativa del texto con el archivo de keywords.

etiquetas_3.0<-function(){
  
  library(rvest)
  
  library(XLConnect)
  
  # Paso 1.- Carga de las URL's desde el sitemap de CLM..- PENDIENTE de pedir el sitemap
  
  site_map<-readWorksheetFromFile("sitemap.xlsx",1,1);etiquetas<-site_map
  keywords<-readWorksheetFromFile("etiquetas.xlsx",1,1)
  
  
  keyI<-keyII<-c()
  
  for (i in 1:NROW(keywords)){
    keyI<-c(keyI,keywords$Keyword[i])
    keyII<-c(keyII,keywords$Etiqueta[i])}
  keywords<-cbind(keyI,keyII)
  
  comp_t<-vec<-scoreII<-c()
  etiquetas$Candidato1<-etiquetas$Candidato2<-c()
  
  for (k in 1:NROW(site_map)){
    # Leemos cualquier contenido de CLM.
    
    tmp <- read_html(site_map$URLs[k])
    tmp<-html_nodes(tmp,"div")
    
    # Sin embargo la estructura de Foros es distinta a la del resto de categorías
    # Por ello, debemos hacer una primera distinción entre el contenido de Foros y el resto.
    # Seleccionamos las URL´s de Foros y las almacenamos en las variables f y b.
    f<-grep("/m-p/",site_map$URLs[k])
    b<-grep("/td-p/",site_map$URLs[k])
    
    #####################    
    # PARTE 1.- FOROS   #
    #####################
    
    if (NROW(f)==1 || NROW(b)==1){t<-grep("messagebodydisplay_0",tmp);tmp<-tmp[t];t<-28}
      
    #############################################
    # PARTE 2.- BLOG, BRICOPEDIA Y PROYECTOS    #
    #############################################
    
    else {t<-max(grep("lia-message-body-content",tmp))}
      
      # Paso 2.- Lectura del html y normalización del texto de Blogs, Bricopedía y Proyecto.
      
      # Lectura:
      texto_html<-html_text(tmp[t])
      
      # Codificación correcta:
      texto_html<-repair_encoding(texto_html,from = "UTF-8");texto_html<-as.character(texto_html);texto_html <- tolower(texto_html)
      
      # Ahora debemos quitar los caracteres propio del html:
      texto_html<-gsub("\n","",texto_html)
      texto_html<-gsub("\t","",texto_html)
      
      # Normalización:  
      texto_split <- strsplit(texto_html, split=" ")
      texto_col <- as.character(unlist(texto_split))
      texto_col <- data.frame(texto_col);names(texto_col)<-"Terms"
      # rm(texto_html)
      rm(texto_split)
      
      texto_fin<-texto_col
      texto_fin$Nchar<-nchar(as.character(texto_fin$Terms))
      texto_fin<-subset(texto_fin,texto_fin$Nchar!=1)
      texto_fin$Terms <- gsub("([[:punct:]])","",texto_fin$Terms)
      texto_fin<-as.data.frame(texto_fin$Terms);names(texto_fin)<-"Terms"
      texto_fin<-subset(texto_fin,texto_fin$Terms!="<NA>")
      texto_fin<-as.data.frame(texto_fin$Terms);names(texto_fin)<-"Terms"
      rm(texto_col)
      
      # Perfecto ahora tenemos el texto correctamente dispuesto.
      
      wb<-loadWorkbook("palabras.xlsx",create=TRUE)
      createSheet(wb,name="Texto")
      writeWorksheet(wb,texto_fin,sheet = "Texto")
      saveWorkbook(wb)
      
      texto_final<-readWorksheetFromFile("palabras.xlsx",1,1)
      file.remove("palabras.xlsx")
#----------------------------------------------------------------------------------------------#
    
    # Paso 3.- Comparativa del texto con el archivo de Keywords.
    
    score<-times_K<-word<-vec<-vec_2.0<-pal<-pal_II<-c()
    for (j in 1:(NROW(texto_final))){vec<-c(vec,texto_final$Terms[j])}
    vec<-subset(vec,vec!="")
    vec<-gsub(" ","",vec)
    tabla<-tabla2<-tabla3<-data.frame("prueba",0,"prueba");names(tabla)<-names(tabla2)<-names(tabla3)<-c("word","Times","Etiqueta")
    
    # Creamos dos dataframes (tabla y tabla2) con los que trabajamos en paralelo.
    
    for (i in 1:NROW(keywords)){index<-grep(keyI[i],vec)
    if (NROW(index)>0)  {pal<-c(pal,rep(keyI[i],times=NROW(index)));pal_II<-c(pal_II,rep(keyII[i],times=NROW(index)))}
    if (NROW(index)==0) {pal<-pal;pal_II<-pal_II}
    times_K[i]<-as.numeric(NROW(index));word<-c(word,keyI[i]);score<-c(score,keyII[i])
    tab<-data.frame(keyI[i],NROW(index),keyII[i]);names(tab)<-c("word","Times","Etiqueta");tabla2<-rbind(tabla2,tab)}
    tabla<-cbind(word,times_K,score)
    tabla<-subset(tabla,tabla[,2]!=0)
    tabla2<-subset(tabla2,tabla2[,2]!=0)
    nr_tab<-NROW(tabla)
    if (nr_tab==0) {etiquetas$Candidato1[k]<-"NadaCandidato_1"
                    etiquetas$Candidato2[k]<-"NadaCandidato_2"}
    
    if (nr_tab>0) {

#----------------------------------------------------------------------------------------------#
      
      # Para etiquetar el contenido aplicamos dos condiciones que deben cumplirse simultáneamente:
      # 1.- Dar la etiqueta al contenido en función de la keyword que más se repite.
      # 2.- Dar la etiqueta en función de la etiqueta que más se repite.
      
#----------------------------------------------------------------------------------------------#    
      
      # Candidato 1.- Keyword más repetida.
      
      d1<-max(tabla2[,2]);index1<-grep(d1,tabla2[,2]);n1<-NROW(index1)
      if (n1==1){etiquetas$Candidato1[k]<-tabla[index1,3]}
      if (n1>1) {tabla_1.0<-table(tabla[index1,3])
      summ_1.0<-as.data.frame(tabla_1.0);names(summ_1.0)<-c("Etiqueta","Frecuencia")
      d1.0<-as.numeric(max(summ_1.0[,2]));index1.0<-grep(d1.0,summ_1.0[,2])
      if (NROW(index1.0) == 1) {etiquetas$Candidato1[k]<-tabla[index1.0,3]}
      if (NROW(index1.0) > 1)  {etiquetas$Candidato1[k]<-"Hay más de dos keyword con igual resultado"}}
      
#----------------------------------------------------------------------------------------------#    
      
      # Candidato 2.- Etiqueta más repetida.
      
      eti<-tabla[,3];eti<-unique(eti)
      summ_2.0<-summa_2.0<-data.frame("prueba",0)
      names(summ_2.0)<-names(summa_2.0)<-c("Etiqueta","Times");vec_2.0<-c()
      
      for (f in 1:NROW(eti)){tab_II<-data.frame(eti[f],NROW(grep(eti[f],pal_II)))
                            names(tab_II)<-names(summa_2.0)
                            summa_2.0<-rbind(summa_2.0,tab_II)}
      summa_2.0<-subset(summa_2.0,summa_2.0$Times!=0)  
      d1<-max(summa_2.0$Times);index1<-grep(d1,summa_2.0$Times)
      if (NROW(index1) == 1){etiquetas$Candidato2[k]<-as.character(summa_2.0$Etiqueta[index1])}
      if (NROW(index1) > 1) {etiquetas$Candidato2[k]<-"Hay más de dos etiquetas con el mismo resultado"}
      
#----------------------------------------------------------------------------------------------#    
      
# Una vez que tenemos los dos candidatos enfrentamos uno a otro.
  
    }        
          if (etiquetas$Candidato1[k] == etiquetas$Candidato2[k]) {etiquetas$Etiquetas[k]<-etiquetas$Candidato1[k]}  
          
          if (etiquetas$Candidato1[k] != etiquetas$Candidato2[k]) {etiquetas$Etiquetas[k]<-"No etiqueta"}   
   
      message("> Etiqueta generada")
      
  }
  
  file.remove("Re_etiquetado.xlsx")
  wb<-loadWorkbook("Re_etiquetado.xlsx",create=TRUE)
  createSheet(wb,name="Etiquetas")
  writeWorksheet(wb,etiquetas,sheet = "Etiquetas")
  saveWorkbook(wb)

  return(tabla2)}

#  ------------------------------------------------------------------------