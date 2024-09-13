
#' Title Extract fasta from file in a loop
#'
#' @param filepath     fasta file to search
#' @param batch        number of rows to read at a time
#' @param skip         number of rows to skip from fasta file
#' @param returnids    logical, TRUE to return identifiers
#' @param fastaids     fasta identifiers to retrieve
#' @param fastafileout file name to save fasta matching ids to
#' @param idfileout    file name to save fasta index to


#'
#' @return saves identifier matches fasta to file and returns a list of number of ids searched, last row searched, ids foundand their row numbers, fasta ids searched for,the file names for the fasta id matches and the fasta index.
#'
#' @examples
#' extract_fasta(filepath="audit.fasta")
#'
#' @export
extract_fasta = function(filepath, batch =1000, skip = 0, returnids=TRUE, fastaids =vector(), fastafileout = "fastaout.txt", idfileout = "fastaindex.txt") {
  if(length(fastaids)>0 & !file.exists(fastafileout)){file.create(fastafileout)}
  con = file(filepath, "r")
  nid = 0
  ids=0
  idr = vector()
  idcodes <-vector()
  present <- batch
  while ( TRUE ) {
    cat(paste0(skip+1,"..,"))

    lines = scan(file = con, what = "list", skip = skip, nlines = batch, multi.line = F)
    # Row numbers for IDS within batch
    id_rownumbers <- grep("^>", lines)
    new_ids <- lines[id_rownumbers]

    if(length(fastaids)>0){
      fasta_rowstart <-id_rownumbers[grep(paste0(fastaids,collapse = "|"),lines[id_rownumbers])]
      fasta <- lines[c(rbind(fasta_rowstart, fasta_rowstart+1))]
      if(length(fasta)>0) write(fasta,file=fastafileout,append=TRUE)
    }

    ids = length(id_rownumbers)
    nid = nid + ids # number of ids
    idr = c(idr, id_rownumbers+skip) # id row numbers
    present = length(lines)

    # save index
    data.table::fwrite(data.table(row = id_rownumbers+skip, idcodes = new_ids), file = idfileout, append=TRUE)
    skip <- skip + present # number of rows
    # All fasta codes
    if(returnids){
      idcodes <- c(idcodes, new_ids)
    }
    if (present < batch) {
      break
    }
  }
  fasta_start = idr
  close(con)
  return(list(ids = nid, finalrow = skip, id_rownumbers = idr, idcodes = idcodes, fastaids = fastaids, fastafileout = fastafileout, index = idfileout))
}

