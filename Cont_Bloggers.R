##########################################
#        CONT. CREADO POR USUARIOS       #
##########################################
#
# Paso 1.- Extracción de datos
#####################################################################################
# Archivo 1.- Usuarios. 

# Fuente de datos: Administrador de la comunidad.

# Rangos:

# Filtro 1.- Activity date range.- Introducir el mes que se quiere analizar.
# Filtro 2.- Registration date.- Desde 01/09/2015 hasta último día del mes analizado.
# Filtro 3.- Por defecto (no tocar)
######################################################################################
# Archivo 2.- Conversaciones creadas por los Usuarios. 

# Fuente de datos: Lithium Social Web.

# Rango: Mes que se está analizando.

# El archivo es un archivo compuesto de Forso y Proyectos.

# Procedimiento: Abrir como .txt delimitado por comas, cambiar la codificación UTF-8 por
# una codificación ANSI y por último abrir el archivo con Excel. Para ver los contenidos que se han 
# creado ese mes filtramos en la hoja de excel en origen.


contenido_user<-function(){
  
  # fichero son los datos extraidos de CLM. 
  # n.-Hoja del Excel donde están los datos.- n=1
  # r=4 para eliminar la cabecera de la tabla.- r=3
  # entrada.- ¿Qué usuarios queremos: Int, ext(Incluidos Bloggers) o Bloggers?

# Introdución de los archivos, debemos decirle que archivos que queremos cargar.
  
  message("Archivos: ")
  ans<-readline("Paso 1.- Introducir el nombre del archivo con la actividad total de los usuarios: " )
  ans1<-getElement(ans,1)
  ans<-readline("Paso 2.- Valor de n (Hoja de Excel que leer): " )
  ans2<-as.numeric(getElement(ans,1))
  ans<-readline("Paso 3.- Valor de r (Línea a partir de la que queremos leer): " )
  ans3<-as.numeric(getElement(ans,1))
  ans<-readline("Paso 4.- Introducir el archivo con los proyectos creados: " )
  ans4<-getElement(ans,1)
  
  message("Tipo de Usuario que queremos analizar: ")
  message("1.- Blogger")
  message("2.- Ghost")
  message("3.- Animador")
    
  ans<-readline("Por favor, indique número : ")
  ans <- getElement(ans,1)
  
  library(XLConnect)
  
    if (ans==1) {
      
      Type_usu<-"Blogger"
    
      # 1.- Cargamos el archivo con el contenido creado por los Bloggers.
      
      datos_user<-readWorksheetFromFile(ans1,ans2,ans3)
      names(datos_user)<-c("Nombre","Email","Roles","Rank","Fecha_registro","Signin_más_reciente","Proyectos","Comentarios_Proyectos","Hilos_Foros","Comentarios_Foros","Articulos_Blog","Comentarios_Blogs","Articulos_Bricopedia","Comentarios_Bricopedia","Post_totales","LiKes_dados","LiKes_recibidos","Total_minutos_online","Signins_totales","Soluciones_aceptadas","Admin.user_Club_Leroy_ID","Admin.user_Leroy_employee_ID","IP_Adress")
      datos_user$Admin.user_Club_Leroy_ID<-datos_user$Admin.user_Leroy_employee_ID<-NULL
      vec_int<-c(grep(Type_usu,datos_user$Roles))
      matriz<-data.frame(datos_user[vec_int,])
      names(matriz)<-names(datos_user)
      
      ## 2.- Cargamos el archivo para extraer las visitas de los contenidos creados por los Bloggers.
      
      datos_cont<-readWorksheetFromFile(ans4,ans2,ans3)
      names(datos_cont)<-c("Contenido","URL","Autor","Fecha_de_publicación","Pag_vistas","Pag_vistas%","Visitas","Visitantes_unicos","Comentarios_recibidos","Likes")
      
      ### 3.- Determino las visitas recibidas por los contenidos creados por los Bloggers.
      
      nomb<-matriz$Nombre
      vis<-data.frame("prueba",0,0)
      for (i in 1:NROW(nomb)){
        selec<-subset(datos_cont,datos_cont$Autor==nomb[i])
                 if (NROW(selec)==1){selec<-data.frame(selec[3],selec[7],selec[9])}
                 if(NROW(selec)>1){visita<-colSums(selec[7])
                    coment<-colSums(selec[9])
                   selec<-data.frame(selec[1,3],visita,coment)}
      names(vis)<-names(selec)<-c("Autor","Visitas_Totales","Comentarios_Recibidos")
        vis<-rbind(vis,selec);vis<-data.frame(vis);vis<-subset(vis,vis$Autor!="prueba")
      }
      
      #### 4.- Creamos un data frame que te englobe estos dos data.frames
      
      matriz$Visitas<-matriz$Comentarios_Rec<-rep(0,times=NROW(matriz))
    
      for (j in 1:NROW(vis)){
        ind<-match(as.factor(vis[j,1]),matriz$Nombre)
        matriz$Visitas[ind]<- vis$Visitas_Totales[j];matriz$Comentarios_Rec[ind]<-vis$Comentarios_Recibidos[j]}

      ##### 5.- Calculamos el scoring y ordenamos el data.frame por el scoring.
      
      matriz$Score<-(matriz$Proyectos)*3+(matriz$Hilos_Foros)*2+(matriz$Comentarios_Foros)*2+matriz$Soluciones_aceptadas+matriz$Comentarios_Proyectos+(matriz$Comentarios_Blog)+(matriz$Comentarios_Bricopedia)+0.02*(matriz$LiKes_recibidos)+(matriz$Visitas)/10000+(matriz$Comentarios_Rec)/25
      matriz<-matriz[order(matriz$Score,decreasing=TRUE),]
      
      
       wb<-loadWorkbook("Contenido_Blogger.xlsx",create=TRUE)
       createSheet(wb,name="Cont_Bloggers")
       writeWorksheet(wb,matriz,sheet = "Cont_Bloggers")
       saveWorkbook(wb)
      }
    else if  (ans==2) {
      
      Type_usu<-"Ghost"
      
      # 1.- Cargamos el archivo con el contenido creado por los Bloggers.
      
      datos_user<-readWorksheetFromFile(ans1,ans2,ans3)
      names(datos_user)<-c("Nombre","Email","Roles","Rank","Fecha_registro","Signin_más_reciente","Proyectos","Comentarios_Proyectos","Hilos_Foros","Comentarios_Foros","Articulos_Blog","Comentarios_Blogs","Articulos_Bricopedia","Comentarios_Bricopedia","Post_totales","LiKes_dados","LiKes_recibidos","Total_minutos_online","Signins_totales","Soluciones_aceptadas","Admin.user_Club_Leroy_ID","Admin.user_Leroy_employee_ID")
      datos_user$Admin.user_Club_Leroy_ID<-datos_user$Admin.user_Leroy_employee_ID<-NULL
      vec_int<-c(grep(Type_usu,datos_user$Roles))
      matriz<-data.frame(datos_user[vec_int,])
      names(matriz)<-names(datos_user)
      
      ## 2.- Cargamos el archivo para extraer las visitas de los contenidos creados por los Bloggers.
      
      datos_cont<-readWorksheetFromFile(ans4,ans2,ans3)
      names(datos_cont)<-c("Contenido","URL","Autor","Fecha_de_publicación","Pag_vistas","Pag_vistas%","Visitas","Visitantes_unicos","Comentarios_recibidos","Likes")
      
      ### 3.- Determino las visitas recibidas por los contenidos creados por los Bloggers.
      
      nomb<-matriz$Nombre
      vis<-data.frame("prueba",0,0)
      for (i in 1:NROW(nomb)){
        selec<-subset(datos_cont,datos_cont$Autor==nomb[i])
        if (NROW(selec)==1){selec<-data.frame(selec[3],selec[7],selec[9])}
        if(NROW(selec)>1){visita<-colSums(selec[7])
        coment<-colSums(selec[9])
        selec<-data.frame(selec[1,3],visita,coment)}
        names(vis)<-names(selec)<-c("Autor","Visitas_Totales","Comentarios_Recibidos")
        vis<-rbind(vis,selec);vis<-data.frame(vis);vis<-subset(vis,vis$Autor!="prueba")
      }
      
      #### 4.- Creamos un data frame que te englobe estos dos data.frames
      
      matriz$Visitas<-matriz$Comentarios_Rec<-rep(0,times=NROW(matriz))
      
      for (j in 1:NROW(vis)){
        ind<-match(as.factor(vis[j,1]),matriz$Nombre)
        matriz$Visitas[ind]<- vis$Visitas_Totales[j];matriz$Comentarios_Rec[ind]<-vis$Comentarios_Recibidos[j]}
      
      ##### 5.- Calculamos el scoring y ordenamos el data.frame por el scoring.
      
      matriz$Score<-(matriz$Proyectos)*3+(matriz$Hilos_Foros)*2+(matriz$Comentarios_Foros)*2+matriz$Soluciones_aceptadas+matriz$Comentarios_Proyectos+(matriz$Comentarios_Blog)+(matriz$Comentarios_Bricopedia)+0.02*(matriz$LiKes_recibidos)+(matriz$Visitas)/10000+(matriz$Comentarios_Rec)/25
      matriz<-matriz[order(matriz$Score,decreasing=TRUE),]
   
      wb<-loadWorkbook("Contenido_Ghost.xlsx",create=TRUE)
      createSheet(wb,name="Cont_Ghost")
      writeWorksheet(wb,matriz,sheet = "Cont_Ghost")
      saveWorkbook(wb)
    }
  
  else if  (ans==3) {
    
    Type_usu<-"Animador"
    
    datos<-readWorksheetFromFile(ans1,ans2,ans3)
    names(datos)<-c("Nombre","Email","Roles","Rank","Fecha_registro","Signin_más_reciente","Proyectos","Comentarios_Proyectos","Hilos_Foros","Comentarios_Foros","Articulos_Blog","Comentarios_Blogs","Articulos_Bricopedia","Comentarios_Bricopedia","Post_totales","LiKes_dados","LiKes_recibidos","Total_minutos_online","Signins_totales","Soluciones_aceptadas","Admin.user_Club_Leroy_ID","Admin.user_Leroy_employee_ID")
    vec_int<-c(grep(Type_usu,datos$Roles))
    matriz<-data.frame(datos[vec_int,])
    
    # Calculo del scoring: Score
    matriz$Score1a<-(matriz$Proyectos)*3+(matriz$Hilos_Foros)*2+(matriz$Comentarios_Foros)*2+matriz$Soluciones_aceptadas+matriz$Comentarios_Proyectos+(matriz$Comentarios_Blog)+(matriz$Comentarios_Bricopedia)
    matriz$Score1b<-0.02*(matriz$LiKes_recibidos)
    matriz$Score<-matriz$Score1a+matriz$Score1b
    matriz$Score1a<-NULL;matriz$Score1b<-NULL
    matriz<-matriz[order(matriz$Score,decreasing=TRUE),]
    
    wb<-loadWorkbook("Contenido_Animador.xlsx",create=TRUE)
    createSheet(wb,name="Cont_Animador")
    writeWorksheet(wb,matriz,sheet = "Cont_Animador")
    saveWorkbook(wb)
  }
    else {message("Algún parametro introducido no es correcto")}
    message("Archivo Excel generado")
    }

############ REVISADO ############