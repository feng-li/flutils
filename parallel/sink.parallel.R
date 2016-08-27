sink.parallel <- function(cl, file)
{
    ## assign("outcon",  file(outfile,  open = "a"),  pos = .GlobalEnv)
    ## assign("errcon",  file(errfile,  open = "a"),  pos = .GlobalEnv)
    ## sink(outcon)
    ## sink(errcon,  type = "message")

    if(missing(file))
    {
        file.gen = "create"
    }
    else
    {
        file.gen = "NULL"
    }


    require("parallel")
    clusterCall(cl = cl, fun = function(file.gen)
    {
        if(file.gen ==  "create")
        {

            JOB_ID <- Sys.getenv("SLURM_JOB_ID")
            if(nchar(JOB_ID)>0)
            {
                JOB.str <- paste("JOB", JOB_ID, ".", sep = "")
            }
            else
            {
                JOB.str <- ""
            }

            file <- paste(JOB.str, system2('hostname', stdout = TRUE),"."
                          as.character(Sys.getpid()), sep = "")
        }
        else
        {
            file <- NULL
        }
        sink(file = file)
    },
    file.gen = file.gen
    )

}



## require(parallel)
## cl = makeCluster(4, type = "MPI")

## sink.parallel(cl = cl)
## parApply(cl,  matrix(1:4),  1,  sin)
## sink.parallel(cl = cl, file = NULL)
