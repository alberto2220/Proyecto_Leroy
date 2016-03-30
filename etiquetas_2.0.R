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

etiquetas_2.0<-function(){
  
  library(rvest)
  
  library(XLConnect)
  
  # Paso 1.- Carga de las URL's desde el sitemap de CLM..- PENDIENTE de pedir el sitemap
  
  site_map<-readWorksheetFromFile("sitemap.xlsx",3,1);etiquetas<-site_map
  keywords<-readWorksheetFromFile("etiquetas.xlsx",1,1)
  
  
  keyI<-keyII<-c()
  
  for (i in 1:NROW(keywords)){
    keyI<-c(keyI,keywords$Keyword[i])
    keyII<-c(keyII,keywords$Etiqueta[i])}
  keywords<-cbind(keyI,keyII)
  
  comp_t<-vec<-scoreII<-c()
  etiquetas$Candidato1<-c();etiquetas$Candidato2<-c()
  
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
    
      if (NROW(f)==1 || NROW(b)==1){
      t<-grep("messagebodydisplay_0",tmp)
      
      texto_html<-html_text(tmp[t])
      texto_html<-repair_encoding(texto_html);texto_html<-as.character(texto_html);texto_html <- tolower(texto_html)
      
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
      etiquetas$Candidato1[k]<-"Foros1";etiquetas$Candidato2[k]<-"Foros2"}
    
    #############################################
    # PARTE 2.- BLOG, BRICOPEDIA Y PROYECTOS    #
    #############################################
    
    else {
      t<-max(grep("lia-message-body-content",tmp))
    
    # Paso 2.- Lectura del html y normalización del texto.
    
    # Lectura:
    texto_html<-html_text(tmp[t])
    
    # Codificación correcta:
    texto_html<-repair_encoding(texto_html);texto_html<-as.character(texto_html);texto_html <- tolower(texto_html)
    
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
    
    wb<-loadWorkbook("palabrasII.xlsx",create=TRUE)
    createSheet(wb,name="Texto")
    writeWorksheet(wb,texto_fin,sheet = "Texto")
    saveWorkbook(wb)
    
#----------------------------------------------------------------------------------------------#

    # Paso 3.- Comparativa del texto con el archivo de Keywords.
    
    texto_final<-readWorksheetFromFile("palabrasII.xlsx",1,1)
    file.remove("palabrasII.xlsx")
    
    score<-times_K<-word<-vec<-vec_2.0<-pal<-c()
    for (j in 1:(NROW(texto_final))){vec<-c(vec,texto_final$Terms[j])}
    vec<-subset(vec,vec!="")
    vec<-gsub(" ","",vec)
    tabla<-data.frame("prueba",0,"prueba");names(tabla)<-c("word","Times","Etiqueta")
    for (i in 1:NROW(keywords)){index<-grep(keyI[i],vec)
    if (NROW(index)>0)  {pal<-c(pal,rep(keyI[i],times=NROW(index)))}
    if (NROW(index)==0) {pal<-pal}
    times_K[i]<-as.numeric(NROW(index));word<-c(word,keyI[i]);score<-c(score,keyII[i])}
    tabla<-cbind(word,times_K,score)
    tabla<-subset(tabla,tabla[,2]!=0)
    # sun1<-table(tabla$word);sun1<-as.data.frame(sun1);names(sun1)<-c("Keyword","Frecuencia")
    # sun2<-table(tabla$score);sun2<-as.data.frame(sun2);names(sun2)<-c("Etiqueta","Frecuencia")
    tabla2<-as.data.frame(tabla)
    nr_tab<-NROW(tabla)
    if (nr_tab==0) {etiquetas$Candidato1[k]<-"NadaCandidato1";etiquetas$Candidato2[k]<-"NadaCandidato2"}
    if (nr_tab>0) {
#----------------------------------------------------------------------------------------------#
    
    # Para etiquetar el contenido aplicamos dos condiciones que deben cumplirse simultáneamente:
    # 1.- Dar la etiqueta al contenido en función de la keyword que más se repite.
    # 2.- Dar la etiqueta en función de la etiqueta que más se repite.

#----------------------------------------------------------------------------------------------#    
 
    # Candidato 1.- Keyword más repetida.
    
    d1<-as.numeric(max(tabla[,2]));index1<-grep(d1,tabla[,2]);n1<-NROW(index1)
        if (n1==1){etiquetas$Candidato1[k]<-tabla[index1,3]}
        if (n1>1) {tabla_1.0<-table(tabla[index1,3])
        summ_1.0<-as.data.frame(tabla_1.0);names(summ_1.0)<-c("Etiqueta","Frecuencia")
        d1.0<-as.numeric(max(summ_1.0[,2]));index1.0<-grep(d1.0,summ_1.0[,2]);n1.0<-NROW(index1.0)
        if (n1.0==1) {etiquetas$Candidato1[k]<-tabla[index1.0,3]}
        if (n1.0>1)  {etiquetas$Candidato1[k]<-"Nada por Keywords"}}
    
#----------------------------------------------------------------------------------------------#    
    
    # Candidato 2.- Etiqueta más repetida.
    
    eti<-tabla[,3];eti<-unique(eti);summ_2.0<-data.frame()
        vec_2.0<-c()
        for (f in 1:NROW(eti)){tabla_2.0<-subset(tabla[,3],tabla[,3]==eti[f]);vec_2.0<-c(vec_2.0,NROW(tabla_2.0))}
        index2<-grep(as.numeric(max(vec_2.0)),vec_2.0);eti2<-eti[index2];n2<-NROW(index2)
        if (n2==1) {etiquetas$Candidato2[k]<-eti[index2]}
        if (n2>1)  {for (f in 1:n2) {summ_2.0<-rbind(summ_2.0,subset(tabla,tabla[,3] == eti2[f]))}
        d2.0<-max(as.numeric((summ_2.0[,2])));index2.0<-grep(d2.0,summ_2.0[,2]);n2.0<-NROW(index2.0) 
        if (n2.0==1) {etiquetas$Candidato2[k]<-summ_2.0[index2.0,3]}
        if (n2.0>1)  {etiquetas$Candidato2[k]<-"Nada por Etiquetas"}}
      
#----------------------------------------------------------------------------------------------#    
  
    # Una vez que tenemos los dos candidatos enfrentamos uno a otro.
        
#     if (cand1 == cand2) {etiquetas$Etiquetas[k]<-cand1}  
#     if (cand1 != cand2) {etiquetas$Etiquetas[k]<-"No etiqueta"}   
        
        
        
             # for (f in 1:NROW(eti)){ind<-grep(eti[f],eti);times_E<-c(times_E,NROW(ind))}
#     tabla_2.0<-cbind(tabla[,3],times_E);tabla_2.0<-unique(tabla_2.0)
    
    # times_E<-as.character(tabla[,3]);times_E<-unique(times_E);tabla_2.0<-table(tabla[,3]);summ_2.0<-as.data.frame(tabla_2.0)
    # tab<-cbind(times_E,summ_2.0[,2]);rownames(tab)<-NULL#;tab<-data.frame(tab);names(tab)<-c("Etiqueta","Frecuencia")

    #     d1<-as.numeric(max(tabla[,3]));index1<-grep(d1,tabla[,2]);n1<-NROW(index1)
#     tabla_2.0<-table(tabla[,3]);summ_2.0<-as.data.frame(tabla_2.0);names(summ_2.0)<-c("Etiq.","Freq")
#     
   # summ_2.0<-data.frame(vec_2.0,freq)
    
    # tabla_2.0<-table(vec_2.0);summ_2.0<-data.frame();;#names(summ_2.0)<-c("Etiqueta","Frecuencia")
    # d2.0<-as.numeric(max(summ_2.0[,2]))#;index_1.0<-grep(d1.0,summ_1.0[,2]);n1.0<-NROW(index_1.0)
       
       # if (tabla[index1,3] == tabla[index_1.0,2]){etiquetas$Etiquetas[k]<-tabla[index1,3]}
       # if (tabla[index1,3] == tabla[index_1.0,2]){etiquetas$Etiquetas[k]<-"Revisar"}
#             
#     
#           if (n1==1){etiquetas$Etiquetas[k]<-tabla[index1,3]}
#           if (n1>1){tabla_1.0<-table(tabla[index1,3])
#                 summ_1.0<-as.data.frame(tabla_1.0);names(summ_1.0)<-c("Etiqueta","Frecuencia")
#                 d1.0<-as.numeric(max(summ_1.0[,2]));index1.0<-grep(d1.0,summ_1.0[,2]);n1.0<-NROW(index1.0)
#                 if (n1.0==1) {etiquetas$Etiquetas[k]<-tabla[index1.0,3]}
#                 if (n1.0>1)  {etiquetas$Etiquetas[k]<-"No es posible etiquetar el contenido"}}
#     
    }
    
    }}
      # file.remove("Re_etiquetado.xlsx")
#     wb<-loadWorkbook("Re_etiquetado.xlsx",create=TRUE)
#     createSheet(wb,name="Etiquetas")
#     writeWorksheet(wb,etiquetas,sheet = "Etiquetas")
#     saveWorkbook(wb)
# #   
  return(tabla2)}


#  ------------------------------------------------------------------------
