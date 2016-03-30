######################################
#  COMPROBACIÓN DE IP REGISTROS CLM  #
######################################

IP_Adress<-function(){
  
library(rvest)
library(XLConnect)
  
# 1.- Cargamos los datos de los registrados semanales.
  ans<-readline("Introduce el archivo con los registrados en CLM: ")
  ans1<-getElement(ans,1)
  
  datos_user<-readWorksheetFromFile(ans1,1,1)
  names(datos_user)<-c("Nombre","Email","Roles","Rank","Fecha_registro","Signin_más_reciente","Proyectos","Comentarios_Proyectos","Hilos_Foros","Comentarios_Foros","Articulos_Blog","Comentarios_Blogs","Articulos_Bricopedia","Comentarios_Bricopedia","Post_totales","LiKes_dados","LiKes_recibidos","Total_minutos_online","Signins_totales","Soluciones_aceptadas","Admin.user_Club_Leroy_ID","Admin.user_Leroy_employee_ID","IP_Adress")

  fp<-"http://stopforumspam.com/ipcheck"
 
  for (i in 1:NROW(datos_user)){
      page<-paste(fp,datos_user$IP_Adress[i],sep="/")
      tmp<-read_html(page)
      tmp<-html_nodes(tmp,"p")
      
      if (length(tmp) == 1){
        datos_user$Spam[i] <- "No Spam"}
      if (length(tmp) > 1){
        datos_user$Spam[i] <- "Spam"}
      
      Sys.sleep(time=1)}
  
  wb<-loadWorkbook("Resultados_Spam.xlsx",create=TRUE)
  createSheet(wb,name="registros")
  writeWorksheet(wb,datos_user,sheet = "registros")
  saveWorkbook(wb)
   message ("Excel generado")
}

#  ------------------------------------------------------------------------


