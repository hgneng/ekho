pub fn get_open_e() -> Vec<f64> {
    let audio = include_str!("../../samples/E2_44100_acousticguitar.txt");
    let data: Vec<f64> = audio
        .lines()
        .filter_map(|s| s.trim().parse::<f64>().ok())
        .collect();
    data
}

pub fn get_degraded_e3() -> Vec<f64> {
    let audio = include_str!("../../samples/E3_44100_viola_degraded_4.txt");
    let data: Vec<f64> = audio
        .lines()
        .filter_map(|s| s.trim().parse::<f64>().ok())
        .collect();
    data
}

pub fn get_undegraded_e3() -> Vec<f64> {
    let audio = include_str!("../../samples/E3_44100_viola_degraded_0.txt");
    let data: Vec<f64> = audio
        .lines()
        .filter_map(|s| s.trim().parse::<f64>().ok())
        .collect();
    data
}

#[cfg(test)]

mod test {
    use super::*;
    use autocorrelation::*;
    use nsdf::*;
    use peak_picking::*;

    #[test]
    fn plot_open_e_autocorrelation() {
        let data = get_open_e();
        let autocorrelation = autocorrelation2(&data, None);

        for i in 0..autocorrelation.len() {
            println!("{:?}", autocorrelation[i]);
        }
    }

    #[test]
    fn plot_open_e_nsdf() {
        let data = get_open_e();
        let autocorrelation = nsdf(&data);

        for i in 0..autocorrelation.len() {
            println!("{:?}", autocorrelation[i]);
        }
    }

    #[test]
    fn plot_undegraded_e3_nsdf() {
        let data = get_undegraded_e3();
        let autocorrelation = nsdf(&data);

        for i in 0..autocorrelation.len() {
            println!("{:?}", autocorrelation[i]);
        }
    }

    #[test]
    fn plot_degraded_e3_nsdf() {
        let data = get_degraded_e3();
        let autocorrelation = nsdf(&data);

        for i in 0..autocorrelation.len() {
            println!("{:?}", autocorrelation[i]);
        }
    }

    #[test]
    fn plot_open_e_with_lag_autocorrelation() {
        let mut data = get_open_e();
        let autocorrelation = autocorrelation2(&data, None);
        let peak_lags = peak_picking_naive(&autocorrelation);

        eprintln!("{:#?}", peak_lags);

        let max_lag = *peak_lags.last().unwrap();
        let max_size = 4096 + max_lag as usize;

        let mut to_plot: Vec<Vec<f64>> = Vec::new();
        data.extend(&vec![0f64; max_size - 4096 + 1]);

        to_plot.push(data.clone());

        for lag in peak_lags {
            let mut vec_for_lag: Vec<f64> = vec![0f64; max_size];

            for i in lag..(lag + 4096) {
                vec_for_lag[i] = data[i - lag];
            }

            to_plot.push(vec_for_lag);
        }

        for i in 0..max_size {
            let mut vals: Vec<f64> = Vec::new();
            for vec in to_plot.iter() {
                vals.push(vec[i]);
            }

            println!(
                "{}",
                vals.iter()
                    .map(|x| format!("{:.2}", x))
                    .collect::<Vec<String>>()
                    .join("\t")
            );
        }
    }

    #[test]
    fn plot_open_e_with_lag_nsdf() {
        let mut data = get_open_e();
        let autocorrelation = nsdf(&data);
        let peak_lags = peak_picking_naive(&autocorrelation);

        eprintln!("{:#?}", peak_lags);

        let max_lag = *peak_lags.last().unwrap();
        let max_size = 4096 + max_lag as usize;

        let mut to_plot: Vec<Vec<f64>> = Vec::new();
        data.extend(&vec![0f64; max_size - 4096 + 1]);

        to_plot.push(data.clone());

        for lag in peak_lags {
            let mut vec_for_lag: Vec<f64> = vec![0f64; max_size];

            for i in lag..(lag + 4096) {
                vec_for_lag[i] = data[i - lag];
            }

            to_plot.push(vec_for_lag);
        }

        for i in 0..max_size {
            let mut vals: Vec<f64> = Vec::new();
            for vec in to_plot.iter() {
                vals.push(vec[i]);
            }

            println!(
                "{}",
                vals.iter()
                    .map(|x| format!("{:.2}", x))
                    .collect::<Vec<String>>()
                    .join("\t")
            );
        }
    }

    #[test]
    fn plot_open_e_with_lag_autocorrelation_peak_picking() {
        let mut data = get_open_e();
        let autocorrelation = autocorrelation2(&data, None);
        let peak_lags = peak_picking_better(&autocorrelation);

        eprintln!("{:#?}", peak_lags);

        let max_lag = *peak_lags.last().unwrap();
        let max_size = 4096 + max_lag as usize;

        let mut to_plot: Vec<Vec<f64>> = Vec::new();
        data.extend(&vec![0f64; max_size - 4096 + 1]);

        to_plot.push(data.clone());

        for lag in peak_lags {
            let mut vec_for_lag: Vec<f64> = vec![0f64; max_size];

            for i in lag..(lag + 4096) {
                vec_for_lag[i] = data[i - lag];
            }

            to_plot.push(vec_for_lag);
        }

        for i in 0..max_size {
            let mut vals: Vec<f64> = Vec::new();
            for vec in to_plot.iter() {
                vals.push(vec[i]);
            }

            println!(
                "{}",
                vals.iter()
                    .map(|x| format!("{:.2}", x))
                    .collect::<Vec<String>>()
                    .join("\t")
            );
        }
    }

    #[test]
    fn plot_open_e_with_lag_nsdf_peak_picking_best() {
        let mut data = get_open_e();
        let autocorrelation = nsdf(&data);
        let mut peak_lags = peak_picking_best(&autocorrelation);

        let mut max_lag = *peak_lags.last().unwrap();
        for i in 1..7 {
            peak_lags.push(max_lag + i * max_lag);
        }
        max_lag = *peak_lags.last().unwrap();

        eprintln!("{:#?}", peak_lags);

        let max_size = 4096 + max_lag as usize;

        let mut to_plot: Vec<Vec<f64>> = Vec::new();
        data.extend(&vec![0f64; max_size - 4096 + 1]);

        to_plot.push(data.clone());

        for lag in peak_lags {
            let mut vec_for_lag: Vec<f64> = vec![0f64; max_size];

            for i in lag..(lag + 4096) {
                vec_for_lag[i] = data[i - lag];
            }

            to_plot.push(vec_for_lag);
        }

        for i in 0..max_size {
            let mut vals: Vec<f64> = Vec::new();
            for vec in to_plot.iter() {
                vals.push(vec[i]);
            }

            println!(
                "{}",
                vals.iter()
                    .map(|x| format!("{:.2}", x))
                    .collect::<Vec<String>>()
                    .join("\t")
            );
        }
    }
}
