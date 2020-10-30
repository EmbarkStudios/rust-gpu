use super::val;

#[test]
fn cf_while() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
    }
}
"#);
}

#[test]
fn cf_while_while() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 20 {
        while i.load() < 10 {
        }
    }
}
"#);
}

#[test]
fn cf_while_while_break() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 20 {
        while i.load() < 10 {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_while_while_if_break() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 20 {
        while i.load() < 10 {
            if i.load() > 10 {
                break;
            }
        }
    }
}
"#);
}

#[test]
fn cf_while_break() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        break;
    }
}
"#);
}

#[test]
fn cf_while_if_break() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while i.load() < 10 {
        if i.load() == 0 {
            break;
        }
    }
}
"#);
}

#[test]
fn cf_if_while() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() == 0 {
        while i.load() < 10 {
        }
    }
}
"#);
}

#[test]
fn cf_if() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {

    }
}
"#);
}
#[test]
fn cf_ifx2() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {

    }
    if i.load() > 1 {

    }
}
"#);
}

#[test]
fn cf_if_else() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {

    } else {

    }
}
"#);
}

#[test]
fn cf_if_elseif_else() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {

    } else if i.load() < 0 {

    } else {
        
    }
}
"#);
}

#[test]
fn cf_if_if() {
    val(r#"
#[allow(unused_attributes)]
#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if i.load() > 0 {
        if i.load() < 10 {

        }
    }
}
"#);
}
