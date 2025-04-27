use std::io::Read;
use std::process::{Child, Command, Stdio};

pub struct SeedGeneratorCommand {
    #[allow(dead_code)]
    child: Child,
    output: Box<dyn Read>,
}

impl SeedGeneratorCommand {
    pub fn new(cmd: &str, inputs: &[&str], use_stdout: bool) -> SeedGeneratorCommand {
        let mut cmd = cmd.to_string();
        let mut consumed = vec![false; inputs.len()];
        for i in 0..inputs.len() {
            let pat = format!("{{{i}}}");
            if cmd.contains(&pat) {
                cmd = cmd.replacen(&pat, inputs[i], 1);
                consumed[i] = true;
            }
        }
        for i in 0..inputs.len() {
            if consumed[i] {
                continue;
            }
            if cmd.contains("{}") {
                cmd = cmd.replacen("{}", inputs[i], 1);
                consumed[i] = true;
            }
        }
        for i in 0..inputs.len() {
            if !consumed[i] {
                cmd = format!("{0} {1}", cmd, inputs[i]);
                consumed[i] = true;
            }
        }
        assert!(consumed.iter().all(|&x| x));
        log::info!("executing seed generator: {cmd}");

        let cmd = cmd.split(" ").collect::<Vec<_>>();
        let (child, output) = if use_stdout {
            let mut child = Command::new(cmd[0])
                .args(&cmd[1..])
                .stdout(Stdio::piped())
                .stderr(Stdio::null())
                .spawn()
                .unwrap();
            let output: Box<dyn Read> = Box::new(child.stdout.take().unwrap());
            (child, output)
        } else {
            let mut child = Command::new(cmd[0])
                .args(&cmd[1..])
                .stdout(Stdio::null())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap();
            let output: Box<dyn Read> = Box::new(child.stderr.take().unwrap());
            (child, output)
        };
        SeedGeneratorCommand { child, output }
    }
}

impl Read for SeedGeneratorCommand {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.output.read(buf)
    }
}
