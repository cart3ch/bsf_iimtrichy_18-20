##### Pre-read assignment
oddeven <- function(number){
  if(number%%1 == 0){
    if(number%%2==0){print("EVEN")}
    else{print("ODD")}
  }
  else {print("NOT AN INTEGER")}
}
