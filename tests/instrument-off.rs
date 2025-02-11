use std::{thread::sleep, time::Duration};
use timbal::{instrument_function, instrument_main};

fn instrumented() {
    instrument_function!();
    sleep(Duration::from_millis(200));
}

#[test]
fn test_instrumented() {
    instrument_main!();

    for _ in 0..10 {
        instrumented();
    }
}
