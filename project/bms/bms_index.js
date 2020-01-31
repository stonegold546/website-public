function textAreaToNum (content) {
    var content = content.toString().replace(/\n/g, ',').replace(/\s/g, ',').split(',');
    return content.filter(function (entry) { return /\S/.test(entry); }).map(Number);
}

// function padElem(elem) {
//     if (!isNaN(elem)) { elem = parseFloat(elem).toFixed(3); }
//     return elem + ' '.repeat(10 - elem.length);
// }

function unravelJSON(json, param, interval, percent = false) {
    var param_dat = json[param];
    var posteriors = param_dat['post'];
    var qntls = jStat.quantiles(posteriors, interval);
    var res = [];
    if (percent) {
        res.push('\t' + (param_dat['mean'] * 100).toFixed(2) + '% (' +
            (qntls[0] * 100).toFixed(2) + '%, ' + (qntls[1] * 100).toFixed(2) + '%), ESS: ' + param_dat['ess'].toFixed(0));
    } else {
        res.push('\t' + param_dat['mean'].toFixed(3) + ' (' + qntls[0].toFixed(3) + ', ' + qntls[1].toFixed(3) +
        '), ESS: ' + param_dat['ess'].toFixed(0));
    }
    res.push([param_dat.mean, param_dat.median, param_dat.sd, qntls[0], qntls[1], param_dat.ess, param_dat.rhat].join(','));
    res.push(posteriors)
    return res;
}

function unravelJsonNoPost(json, param, percent = false) {
    var param_dat = json[param];
    var res = [];
    if (percent) {
        res.push('\t' + (param_dat['mean'].toFixed(3) * 100).toFixed(2) + '% (' +
            (param_dat['int.lo'] * 100).toFixed(2) + '%, ' + (param_dat['int.hi'] * 100).toFixed(2) + '%), ESS: ' + param_dat['ess'].toFixed(0));
    } else {
        res.push('\t' + param_dat['mean'].toFixed(3) + ' (' + param_dat['int.lo'].toFixed(3) + ', ' + param_dat['int.hi'].toFixed(3) +
            '), ESS: ' + param_dat['ess'].toFixed(0));
    }
    res.push([param_dat.mean, param_dat.median, param_dat.sd, param_dat['int.lo'], param_dat['int.hi'], param_dat.ess, param_dat.rhat].join(','));
    return res;
}

var cleanParams = {
    ttest: {
        m_diff: 'mean_diff', st_ratio: 'sd_ratio',
        m1: 'mean1', m0: 'mean2', st1: 'scale1', st0: 'scale2',
        nu: 'df'
    },
    twogrpbin: {
        m0: 'mean2_logit', m_diff: 'mdiff_logit', 'means_logit[2]': 'mean1_logit',
        odds_ratio: 'odds_ratio', 'means_prob[1]': 'mean2_prob', 'means_prob[2]': 'mean1_prob',
        prob_ratio: 'prob_ratio', prob_diff: 'prob_diff'
    }
}

function createDumpFile (dump, post, params, name) {
    var divisor = post.length / 4;
    post = post.map(function (row, idx) {
        return parseInt(idx / divisor + 1) + ',' + row.join(',');
    }).join('\n')
    dump += '\n\n' + 'POSTERIOR DRAWS\n\n' +
        'chain,' + params.join(',') + '\n' + post + '\n';
    var blob = new Blob([dump.replace('\n', '\r\n')], { type: 'text/csv' })
    var elem = window.document.createElement('a')
    elem.href = window.URL.createObjectURL(blob)
    elem.download = name + '.csv'
    document.body.appendChild(elem)
    elem.click()
    document.body.removeChild(elem)
    window.URL.revokeObjectURL(elem.href)
}

function downloadImage(chart, name) {
    var link = document.createElement('a');
    link.href = 'data:application/octet-stream;base64,' + chart;
    link.download = name + '.png';
    document.body.appendChild(link)
    link.click();
    document.body.removeChild(link);
    window.URL.revokeObjectURL(link.href);
}

var loader_div = new Vue({
    el: '#loader_div',
    data: { isActive: false }
})

const ts_form = new Vue({
    el: '#ts_form',
    data: {
        base_message: '\
                   Mean difference:\n\
Ratio of group standard deviations:\n\
       Mean of group 1 (treatment):\n\
         Mean of group 2 (control):\n\
      Scale of group 1 (treatment):\n\
        Scale of group 2 (control):\n\
   t dist. degrees of freedom (df):\n\
\n\
Note: df close to 0 suggests outliers in data. As df increases, outliers are less prominent in data. df >= 30 is hardly distinguishable from normality.\n\
Scale estimate is proportional but not equal to standard deviation. ESS is effective sample size.',
        message: this.base_message, max_sr: 3,
        sd_m: 5, sd_st: 1, nu_choice: 0, errors: [],
        // ts_y1: '56.8, 60.1, 50.3, 48.8, 61.0, 47.3, 60.3, 58.8, 48.2, 50.8, 43.6, 58.8, 51.4, 55.4, 62.0, 60.4, 63.2, 65.4, 33.5, 57.0, 43.9, 55.7, 56.4',
        // ts_y0: '59.6, 50.0, 52.8, 57.5, 51.1, 47.7, 52.4, 35.7, 50.0, 43.9, 53.7, 51.4, 55.2, 46.0, 52.8, 39.0, 44.1, 58.6, 52.0, 56.7, 50.3, 58.4, 53.9, 46.4, 54.4, 55.3, 45.2',
        ts_y1: '101, 100, 102, 104, 102, 97, 105, 105, 98, 101, 100, 123, 105, 103, 100, 95, 102, 106, 109, 102, 82, 102, 100, 102, 102, 101, 102, 102, 103, 103, 97, 97, 103, 101, 97, 104, 96, 103, 124, 101, 101, 100, 101, 101, 104, 100, 101',
        ts_y0: '99, 101, 100, 101, 102, 100, 97, 101, 104, 101, 102, 102, 100, 105, 88, 101, 100, 104, 100, 100, 100, 101, 102, 103, 97, 101, 101, 100, 101, 99, 101, 100, 100, 101, 100, 99, 101, 100, 102, 99, 100, 99',
        ts_max_diff: 5, ts_max_st_r: 3,
        ts_n_iter: 1500, ts_int: 95, n1: null, n0: null
    },
    methods: {
        checkForm: function (e) {
            e.preventDefault();
            loader_div.isActive = true;
            this.message = this.base_message;
            this.errors = [];
            this.n1 = null;
            this.n0 = null;

            var y1 = textAreaToNum(this.ts_y1);
            var y0 = textAreaToNum(this.ts_y0);
            var anyNaN1 = y1.map(isNaN).reduce((a, b) => a + b, 0);
            var anyNaN0 = y0.map(isNaN).reduce((a, b) => a + b, 0);

            if (anyNaN1) {
                this.errors.push('Invalid values for group 1.');
            }
            if (anyNaN0 > 0) {
                this.errors.push('Invalid values for group 2');
            }
            if (y1.length < 4 || y0.length < 4) {
                this.errors.push('Minimum of four values required per group.');
            }

            if (this.errors.length) {
                loader_div.isActive = false;
                return true;
            }

            this.n1 = y1.length;
            this.n0 = y0.length;

            var interval_x = (1 - parseFloat(this.ts_int) / 100) / 2;
            interval_x = [interval_x, 1 - interval_x];
            var params = ['m_diff', 'st_ratio', 'm1', 'm0', 'st1', 'st0', 'nu'];

            axios.post(
                // 'http://localhost:8000/two_sample_test',
                'https://uanhoro1.pythonanywhere.com/two_sample_test',
                {
                    params: {
                        y1: y1, y0: y0, sd_m: this.sd_m, max_diff: parseFloat(this.ts_max_diff), sd_st: this.sd_st,
                        max_st_r: parseFloat(this.ts_max_st_r), nu_choice: this.nu_choice, n_iter: parseInt(this.ts_n_iter)
                    }
                }
            )
                .then(response => {
                    results = response.data;
                    posteriors = [];
                    paramsList = [];
                    messageSplit = this.base_message.split('\n');
                    dumpText = ['statistic', 'mean', 'median', 'sd', 'quantile_interval_lo', 'quantile_interval_hi', 'effective_sample_size', 'rhat'].join(',') + '\n';
                    for (let i = 0; i < 7; i++) {
                        res = unravelJSON(results, params[i], interval_x);
                        messageSplit[i] += res[0];
                        paramsList.push(cleanParams.ttest[params[i]]);
                        dumpText += paramsList[i] + ',' + res[1] + '\n';
                        posteriors.push(res[2]);
                    }

                    downloadImage(results.mean_hash, 'mean_diff');
                    downloadImage(results.sc_hash, 'scale_ratio');
                    downloadImage(results.rk_hash, 'rank_plots');

                    posteriors = math.transpose(posteriors);
                    createDumpFile(dumpText, posteriors, paramsList, 'two_sample_summary');

                    this.message = messageSplit.join('\n');
                    loader_div.isActive = false;
                })
                .catch(error => {
                    console.log(error)
                    loader_div.isActive = false;
                });
        },
    }
})

const tsb_form = new Vue({
    el: '#tsb_form',
    data: {
        base_message: '\
                       Odds ratio:\n\
                       Risk ratio:\n\
                  Risk difference:\n\
Average prob. group 1 (treatment):\n\
  Average prob. group 2 (control):\n\
\n\
Note: Risk ratio is the average probability of group 1 divided by the average probability of group 2. \
Risk difference is the average probability of group 1 minus the average probability of group 2. ESS is effective sample size.',
        message: this.base_message, errors: [],
        tsb_s1: 1513, tsb_f1: 512, tsb_s2: 1459, tsb_f2: 554,
        tsb_extreme: 'false',
        tsb_sd_m: 3, tsb_sd_m_diff: 2, tsb_n_iter: 2000, tsb_int: 95,
        tsb_t1: null, tsb_t2: null
    },
    methods: {
        checkForm: function (e) {
            e.preventDefault();
            loader_div.isActive = true;
            this.message = this.base_message;
            this.errors = [];
            this.tsb_t1 = null;
            this.tsb_t1 = null;

            var success = [parseInt(this.tsb_s2), parseInt(this.tsb_s1)];
            var total = [parseInt(this.tsb_s2) + parseInt(this.tsb_f2), parseInt(this.tsb_s1) + parseInt(this.tsb_f1)];

            if (this.errors.length) {
                loader_div.isActive = false;
                return true;
            }

            this.tsb_t1 = total[1];
            this.tsb_t2 = total[0];

            var interval_x = (1 - parseFloat(this.ts_int) / 100) / 2;
            interval_x = [interval_x, 1 - interval_x];
            var params = ['odds_ratio', 'prob_ratio', 'prob_diff', 'means_prob[2]', 'means_prob[1]'];

            this.tsb_sd_m = this.tsb_extreme == 'true' ? 5 : 2

            axios.post(
                // 'http://localhost:8000/two_sample_binary',
                'https://uanhoro1.pythonanywhere.com/two_sample_binary',
                {
                    params: {
                        success: success, total: total,
                        sd_m: parseFloat(this.tsb_sd_m), sd_m_diff: parseFloat(this.tsb_sd_m_diff),
                        interval: parseInt(this.tsb_int), n_iter: parseInt(this.tsb_n_iter)
                    }
                }
            )
                .then(response => {
                    results = response.data;
                    paramsList = [];
                    messageSplit = this.base_message.split('\n');
                    dumpText = ['statistic', 'mean', 'median', 'sd', 'quantile_interval_lo', 'quantile_interval_hi', 'effective_sample_size', 'rhat'].join(',') + '\n';
                    for (let i = 0; i < params.length; i++) {
                        res = [];
                        if ([3, 4].includes(i)) {
                            res = unravelJsonNoPost(results, params[i], true);
                        } else {
                            res = unravelJsonNoPost(results, params[i]);
                        }
                        messageSplit[i] += res[0];
                        paramsList.push(cleanParams.twogrpbin[params[i]]);
                        dumpText += paramsList[i] + ',' + res[1] + '\n';
                    }

                    posteriors = ['m0', 'm_diff', 'odds_ratio', 'prob_ratio', 'prob_diff'].map(function (param) {
                        return results[param]['post']
                    })
                    downloadImage(results.or_hash, 'odds_ratio');
                    downloadImage(results.pr_hash, 'prob_ratio');
                    downloadImage(results.pd_hash, 'prob_diff');
                    downloadImage(results.rk_hash, 'rank_plots');

                    posteriors = math.transpose(posteriors);
                    createDumpFile(dumpText, posteriors,
                        ['mean2_logit (intercept)', 'mdiff_logit (effect)', 'odds_ratio', 'prob_ratio', 'prob_diff'], 'two_way_table');

                    this.message = messageSplit.join('\n');
                    loader_div.isActive = false;
                })
                .catch(error => {
                    console.log(error)
                    loader_div.isActive = false;
                });
        },
    }
})
