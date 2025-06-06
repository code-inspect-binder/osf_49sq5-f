# neatStats usage example: pipeline from raw data to reportable statistics

library('neatStats') # load package

setwd(path_neat('example_data')) # set the result files' folder path as current working directory

# read in all text files at working directory:
# all_data = read_dir(
#     pattern = "\\.txt$",
#     stringsAsFactors = FALSE,
#     fill = TRUE,
#     header = TRUE
# )

# look at the columns and what they contain:
# str(all_data)

# look at data range, potential outliers:
# peek_neat(all_data, 'rt')
# the same per various groupings:
#' peek_neat(
#'     dat = all_data,
#'     values = 'rt',
#'     group_by = 'response',
#'     round_to = 1
#' )
#' peek_neat(all_data, 'rt', c('color', 'valence'),
#'           round_to = 1)

# histogram and Q-Q plots for the same:
# peek_neat(all_data, 'rt', c('color', 'valence'), f_plot = plot_neat)
# peek_neat(all_data, 'rt', c('color', 'valence'), f_plot = ggpubr::ggqqplot)

filenames = list.files(pattern = "^expsim_color_valence_.*\\.txt$") # get all result file names

for (file_name in enum(filenames)) {
    # print current file name - just to monitor the process
    cat(file_name, fill = TRUE)
    
    # read the data with given file name
    # (any single file can also be separately read here for convenience)
    # (e.g. set file_name = c(0, "expsim_color_valence_mixed_61.txt"))
    subject_data = read.table(
        file_name[2],
        header = TRUE,
        stringsAsFactors = FALSE,
        fill = TRUE
    )

    # check if trial number is correct
    if (nrow(subject_data) != 100) {
        stop("unexpected trial number: ", nrow(subject_data))
    }

    # now aggregate rt data per type
    rts = aggr_neat(
        subject_data,
        rt,
        group_by = c('color', 'valence'),
        method = mean,
        prefix = 'rt',
        filt = (rt > 150 & response == 'correct')
    )

    # same with error rates; get ratio of 'incorrect'
    ers = aggr_neat(
        subject_data,
        response,
        group_by = c('color', 'valence'),
        method = 'incorrect',
        prefix = 'er',
        filt = (response %in% c('correct', 'incorrect'))
    )
    
    # get overall error rate
    er_overall = aggr_neat(subject_data,
                           response,
                           method = 'incorrect',
                           filt = (response %in% c('correct', 'incorrect')))$aggr_value

    # merge subject data
    rbind_loop(
        subjects_merged,
        subject_id = subject_data$subject_num[1],
        condition = subject_data$condition[1],
        gender = subject_data$gender[1],
        age = subject_data$age[1],
        er_overall = er_overall,
        rts, # will be converted to row
        ers # will be converted to row
    )
}

# data is ready for analysis as subjects_merged

# list column names to take a look and easily copy
str(subjects_merged)

# check error rates
peek_neat(subjects_merged,
          values = 'er_overall',
          f_plot = plot_neat)

# exclude subjects with overall error rate larger than 20%
data_final = excl_neat(subjects_merged, er_overall < 0.20,
                       group_by = 'condition')

# look at rt data range and distribution, potential outliers
peek_neat(
    data_final,
    values = c(
        'rt_green_negative',
        'rt_red_negative',
        'rt_green_positive',
        'rt_red_positive'
    ),
    group_by = 'condition',
    f_plot = plot_neat
)

# collapsing per condition
peek_neat(
    data_final,
    values = c(
        'rt_green_negative',
        'rt_red_negative',
        'rt_green_positive',
        'rt_red_positive'
    ),
    group_by = 'condition',
    collapse = mean,
    f_plot = plot_neat,
    round_to = 1
)

# print demographics
dems_neat(data_final, group_by = 'condition')

# now ANOVA on RTs for the main question: Color/Valence/Group interaction
# with basic factorial plot of mean rt means 
# (95% CI for error bars by default)
anova_neat(
    data_final,
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    between_vars = 'condition',
    plot_means = TRUE,
    norm_tests = 'all',
    norm_plots = TRUE,
    var_tests = TRUE
)

# follow-up in 'separate' condition
anova_neat(
    data_final[data_final$condition == 'separate',],
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    bf_added = TRUE,
    norm_tests = 'all',
    norm_plots = TRUE
)

# follow-up in 'mixed' condition
anova_neat(
    data_final[data_final$condition == 'mixed', ],
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    bf_added = TRUE,
    norm_tests = 'all',
    norm_plots = TRUE
)

# follow-up t tests for 'mixed' condition
subjects_mx = excl_neat(data_final, condition == 'mixed')

# negative
t_neat(
    subjects_mx$rt_green_negative,
    subjects_mx$rt_red_negative,
    pair = TRUE,
    norm_tests = 'all',
    norm_plots = TRUE,
    bf_added = TRUE
)
t_neat(
    subjects_mx$rt_green_negative,
    subjects_mx$rt_red_negative,
    pair = TRUE,
    nonparametric = TRUE,
    bf_added = TRUE,
)

# positive
t_neat(subjects_mx$rt_green_positive,
       subjects_mx$rt_red_positive,
       pair = TRUE,
       norm_tests = 'all',
       norm_plots = TRUE,
       bf_added = TRUE)

# table to show basic data
table_neat(
    list(
        aggr_neat(data_final, rt_green_negative, round_to = 0),
        aggr_neat(data_final, rt_green_positive, round_to = 0),
        aggr_neat(data_final, rt_red_negative, round_to = 0),
        aggr_neat(data_final, rt_red_positive, round_to = 0),
        aggr_neat(data_final, data_final$er_green_negative * 100),
        aggr_neat(data_final, data_final$er_green_positive * 100),
        aggr_neat(data_final, data_final$er_red_negative * 100),
        aggr_neat(data_final, data_final$er_red_positive * 100)
    ),
    group_by = 'condition',
    to_clipboard = TRUE
)
