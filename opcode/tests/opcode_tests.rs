use anyhow::Error;
use opcode::{concat_instructions, make, Opcode};

#[test]
fn test_make() -> Result<(), Error> {
    let tests = vec![(
        Opcode::OpConst,
        vec![65534],
        vec![Opcode::OpConst as u8, 255, 254],
    )];

    for (opcode, operands, expected) in tests {
        let instruction = make(opcode, &operands);
        assert_eq!(instruction.0, expected);
    }

    Ok(())
}

#[test]
fn test_instructions_string() -> Result<(), Error> {
    let instructions = vec![
        make(Opcode::OpConst, &vec![1]),
        make(Opcode::OpConst, &vec![2]),
        make(Opcode::OpConst, &vec![65535]),
    ];

    let expected = "0000 OpConst 1\n0003 OpConst 2\n0006 OpConst 65535\n";

    let concatted = concat_instructions(&instructions);

    assert_eq!(concatted.to_string(), expected);

    Ok(())
}
