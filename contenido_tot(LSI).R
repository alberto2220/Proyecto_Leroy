#########################################
#      CONT. CREADO GENERAL EN CLM      #
#########################################

# Datos extraidos de LSI
# (Contenido total creado en la CLM) Scrip válido para contar la participación de los usuarios en Foros distinguiendo
# el tipo de usuario.

contenidoCLM_LSI<-function(fichero,n,r,entrada){
  # n.-Hoja del Excel donde están los datos.
  # r=2 para eliminar la cabecera de la tabla.
  # entrada.- ¿Qué usuarios queremos: Int, ext(Incluidos Bloggers) o Bloggers?
  library(XLConnect)
  datos<-readWorksheetFromFile(fichero,sheet=n,startRow=r)
  names(datos)<-c("Miembros","Dominio","URL","Roles","Ranking","Páginas_Vistas","Páginas_Vistas_porc","Visitas","Hilos","Comentarios","LiKes","Likes_Recibidos","Soluciones","Soluciones_autorizadas")
  datos$URL<-datos$Roles<-datos$Páginas_Vistas<-datos$Páginas_Vistas_porc <-datos$Likes_Recibidos<-datos$Soluciones_autorizadas<-NULL 
  datos$Usuarios_interaccionan<-datos$Hilos+datos$Comentarios+datos$LiKes
  datos$Usuarios_creadores<-datos$Hilos+datos$Comentarios
  matrix_int<-matrix_ext<-data.frame("prueba","prueba","prueba",0,0,0,0,0,0,0)
  names(matrix_int)<-names(matrix_ext)<-names(datos)
  
  for (i in 1:NROW(datos)){
    if (datos[i,2]=="leroymerlin.es" || datos[i,2]=="ext.leroymerlin.es" || datos[i,2]== "leroy.com" || datos[i,2]=="eroymerlin.es" || datos[i,2]=="leroymeril.es" || datos[i,2]=="leroymerlin.pt" || datos[i,2]=="lithium.com" || datos[i,2]=="socialnautas.es" || datos[i,2]=="the-cocktail.com"|| datos[i,5]=="Animador"|| datos[i,5]=="Colaborador"){
      matrix_int<-rbind(datos[i,],matrix_int)}
    else {matrix_ext<-rbind(datos[i,],matrix_ext)}
  }
  if (entrada=="int") {
    usuarios_int<-colSums(matrix_int[,4:8])
    usuarios<-NROW(matrix_int)
    usuarios_int_interaccionan<-NROW(subset(matrix_int,matrix_int$Usuarios_interaccionan!=0))
    usuarios_int_Creadores<-NROW(subset(matrix_int,matrix_int$Usuarios_creadores!=0))
    contenido<-usuarios_int[2]+usuarios_int[3]
    usuarios_int<-c(fichero,usuarios,usuarios_int_Creadores,usuarios_int_interaccionan,usuarios_int,contenido)
    names(usuarios_int)<-c("Fichero","Total_usuarios_int","Usuarios_int_creadores","Usuarios_int_interaccionan","Visitas","Hilos","Comentarios","Likes","Soluciones","Contenido UGC Creado")
    usuarios_int<-data.frame(usuarios_int)
    return(usuarios_int)}
  else  if (entrada=="ext") {
    usuarios_ext<-colSums(matrix_ext[,4:8])
    usuarios<-NROW(matrix_ext)
    usuarios_ext_interaccionan<-NROW(subset(matrix_ext,matrix_ext$Usuarios_interaccionan!=0))
    usuarios_ext_Creadores<-NROW(subset(matrix_ext,matrix_ext$Usuarios_creadores!=0))
    contenido<-usuarios_ext[2]+usuarios_ext[3]
    usuarios_ext<-c(fichero,usuarios,usuarios_ext_Creadores,usuarios_ext_interaccionan,usuarios_ext,contenido)
    names(usuarios_ext)<-c("Fichero","Total_usuarios_ext","Usuarios_ext_creadores","Usuarios_ext_interaccionan","Visitas","Hilos","Comentarios","Likes","Soluciones","Contenido UGC Creado")
    usuarios_ext<-data.frame(usuarios_ext)
    return(usuarios_ext)}
}

############ REVISADO ############