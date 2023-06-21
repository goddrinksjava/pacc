use std::fmt;

#[derive(Debug)]
pub enum Mnemo {
    LIT,
    LOD,
    STO,
    JMP,
    JMC,
    CAL,
    OPR,
}

impl fmt::Display for Mnemo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub enum OPR {
    Fin,
    Retorno,
    Suma,
    Resta,
    Multiplicacion,
    Division,
    Modulo,
    Exponente,
    MenosUnitario,
    MenorQue,
    MayorQue,
    MenorIgualQue,
    MayorIgualQue,
    Diferente,
    Igual,
    O,
    Y,
    No,
    LimpiaPantalla,
    LeeEntrada,
    Print,
    PrintLn,
}

impl OPR {
    fn value(&self) -> usize {
        match self {
            OPR::Fin => 0,
            OPR::Retorno => 1,
            OPR::Suma => 2,
            OPR::Resta => 3,
            OPR::Multiplicacion => 4,
            OPR::Division => 5,
            OPR::Modulo => 6,
            OPR::Exponente => 7,
            OPR::MenosUnitario => 8,
            OPR::MenorQue => 9,
            OPR::MayorQue => 10,
            OPR::MenorIgualQue => 11,
            OPR::MayorIgualQue => 12,
            OPR::Diferente => 13,
            OPR::Igual => 14,
            OPR::O => 15,
            OPR::Y => 16,
            OPR::No => 17,
            OPR::LimpiaPantalla => 18,
            OPR::LeeEntrada => 19,
            OPR::Print => 20,
            OPR::PrintLn => 21,
        }
    }
}

impl fmt::Display for OPR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

pub struct PL0Instruction {
    pub mnemo: Mnemo,
    pub dir1: String,
    pub dir2: String,
}

impl fmt::Display for PL0Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}, {}", self.mnemo, self.dir1, self.dir2)
    }
}

impl PL0Instruction {
    pub fn lod(iden: String) -> PL0Instruction {
        PL0Instruction {
            mnemo: Mnemo::LOD,
            dir1: iden,
            dir2: "0".to_string(),
        }
    }

    pub fn sto(iden: String) -> PL0Instruction {
        PL0Instruction {
            mnemo: Mnemo::STO,
            dir1: "0".to_string(),
            dir2: iden,
        }
    }

    pub fn opr(opr: OPR) -> PL0Instruction {
        PL0Instruction {
            mnemo: Mnemo::OPR,
            dir1: "0".to_string(),
            dir2: opr.to_string(),
        }
    }

    pub fn jmp(pos: String) -> PL0Instruction {
        PL0Instruction {
            mnemo: Mnemo::JMP,
            dir1: "0".to_string(),
            dir2: pos,
        }
    }

    pub fn jmc(cond: String, pos: String) -> PL0Instruction {
        PL0Instruction {
            mnemo: Mnemo::JMC,
            dir1: cond,
            dir2: pos,
        }
    }

    pub fn lit(lit: String) -> PL0Instruction {
        PL0Instruction {
            mnemo: Mnemo::LIT,
            dir1: lit,
            dir2: "0".to_string(),
        }
    }
}
