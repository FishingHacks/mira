#[derive(Debug, Clone, Copy, Default)]
pub struct BoxDrawingChars {
    pub bottom_left: char,
    pub vertical_right: char,
    pub vertical: char,
}

pub const NORMAL: BoxDrawingChars = BoxDrawingChars {
    bottom_left: '└',
    vertical_right: '├',
    vertical: '│',
};

pub const BOLD: BoxDrawingChars = BoxDrawingChars {
    bottom_left: '┗',
    vertical_right: '┣',
    vertical: '┃',
};

pub const DOUBLE: BoxDrawingChars = BoxDrawingChars {
    bottom_left: '╚',
    vertical_right: '╠',
    vertical: '║',
};

pub const ROUND: BoxDrawingChars = BoxDrawingChars {
    bottom_left: '╰',
    vertical_right: '├',
    vertical: '│',
};
