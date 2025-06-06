next_subject = function(sub_num) {
    N = 100
    sub_dat = data.frame(
        subject_num = toString(sub_num),
        condition = sample(c('separate', 'mixed'), 1),
        rt = rnorm(n = N, mean = 450, sd = 150),
        response = sample(
            c(rep('correct', 9), 'incorrect', 'tooslow'),
            size = N,
            replace = TRUE
        ),
        color = sample(c('red', 'green'), size = N, replace = TRUE),
        valence = sample(
            c('positive', 'negative'),
            size = N,
            replace = TRUE
        ),
        age = sample(18:30, 1),
        gender = sample(c(1, 2), 1)
    )
    if (sub_dat$condition[1] == 'mixed') {
        sub_dat$age = sub_dat$age + sample(c(0, 0, 0, 0, 1), 1)
        green_neg = (sub_dat$color == 'green' &
                         sub_dat$valence == 'negative')
        sub_dat$rt[green_neg] = sub_dat$rt[green_neg] + rnorm(n = length(sub_dat$rt[green_neg]),
                                                              mean = 43,
                                                              sd = 30)
        sub_dat$response[green_neg] = sample(
            c(rep('correct', 6), 'incorrect', 'tooslow'),
            size = length(sub_dat$response[green_neg]),
            replace = TRUE
        )
        red_pos = (sub_dat$color == 'red' &
                       sub_dat$valence == 'positive')
        sub_dat$rt[red_pos] = sub_dat$rt[red_pos] + rnorm(n = length(sub_dat$rt[red_pos]),
                                                          mean = 37,
                                                          sd = 30)
        sub_dat$response[red_pos] = sample(
            c(rep('correct', 6), 'incorrect', 'tooslow'),
            size = length(sub_dat$response[red_pos]),
            replace = TRUE
        )
    }
    neg = (sub_dat$valence == 'negative')
    sub_dat$rt[neg] = sub_dat$rt[neg] + rnorm(n = length(sub_dat$rt[neg]),
                                              mean = 40,
                                              sd = 25)
    sub_dat$response[neg] = sample(
        c(rep('correct', 6), 'incorrect', 'tooslow'),
        size = length(sub_dat$response[neg]),
        replace = TRUE
    )
    sub_dat$rt = sub_dat$rt + sample(0:150, 1)
    if (sub_dat$condition[1] == 'separate') {
        sub_dat =  sub_dat[order(sub_dat$color, decreasing = sample(c(TRUE, FALSE), size = 1)),]
    }
    return(sub_dat)
}

pathstart = neatStats::path_neat('example_data/expsim_color_valence_')
for (sub_id in 1:180) {
    cat(sub_id, ' ')
    dat_out = next_subject(sub_id)
    dat_out$rt = abs(dat_out$rt)
    dat_out$rt[dat_out$response == 'tooslow'] = NA
    write.table(
        dat_out,
        file = paste0(pathstart, dat_out$condition[1], '_', sub_id, '.txt'),
        sep = "\t",
        quote = F,
        row.names = F
    )
}
