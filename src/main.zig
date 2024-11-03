const std = @import("std");
// const BitReader = std.io.BitReader(.little, std.fs.File.Reader);
// const BitWriter = std.io.BitWriter(.little, std.fs.File.Writer);
// const Reader = std.io.Reader();
const ArrayList = std.ArrayList;

const Word = union(enum) {
    pure: PureWord,
    parent: ParentWord,
    source: SourceWord,
};

const PureWord = struct {
    name: u8,
    prob:f64,
};

pub fn recursePure(self: *const PureWord, words: *ArrayList(u8), codes: *ArrayList(ArrayList(bool)), running_code: *ArrayList(bool), alloc: anytype) std.mem.Allocator.Error!ArrayList(u8) {
    var words_down = ArrayList(u8).init(alloc);
    try words_down.append(self.name);
    try codes.append(running_code.*);
    try words.append(self.name);
    return words_down;
}

const ParentWord = struct {
    child0: *const Word,
    child1: *const Word,
    name: *const ArrayList(u8)
};

pub fn recurseParent(parent: *const ParentWord, words: *ArrayList(u8), codes: *ArrayList(ArrayList(bool)),
    running_code: *ArrayList(bool), alloc: anytype) std.mem.Allocator.Error!ArrayList(u8) {
    var words_down = ArrayList(u8).init(alloc);
    var code0 = ArrayList(bool).init(alloc);
    try code0.ensureTotalCapacity(running_code.items.len + 1);
    for (running_code.items) |item| {
        try code0.append(item);
    }
    try code0.append(false);
    const words0 = try recurseWord(parent.child0, words, codes, &code0, alloc);
    try words_down.appendSlice(words0.items);
    var code1 = ArrayList(bool).init(alloc);
    try code1.ensureTotalCapacity(running_code.items.len + 1);
    for (running_code.items) |item| {
        try code1.append(item);
    }
    try code1.append(true);
    const words1 = try recurseWord(parent.child1, words, codes, &code1, alloc);
    try words_down.appendSlice(words1.items);
    var parent_list = parent.name.*;
    try parent_list.appendSlice(words_down.items);
    return words_down;
}

const SourceWord = struct {
    child0: *const Word,
    child1: *const Word,
    name: *const ArrayList(u8)
};

pub fn recurseSource(self: *const SourceWord, words: *ArrayList(u8), codes: *ArrayList(ArrayList(bool)),
    running_code: *ArrayList(bool), alloc: anytype) std.mem.Allocator.Error!ArrayList(u8) {
    var words_down = ArrayList(u8).init(alloc);
    var code0 = ArrayList(bool).init(alloc);
    try code0.ensureTotalCapacity(running_code.items.len + 1);
    for (running_code.items) |item| {
        try code0.append(item);
    }
    try code0.append(false);
    const words0 = try recurseWord(self.child0, words, codes, &code0, alloc);
    try words_down.appendSlice(words0.items);
    var code1 = ArrayList(bool).init(alloc);
    try code1.ensureTotalCapacity(running_code.items.len + 1);
    for (running_code.items) |item| {
        try code1.append(item);
    }
    try code1.append(true);
    const words1 = try recurseWord(self.child1, words, codes, &code1, alloc);
    try words_down.appendSlice(words1.items);
    return words_down;
}

pub fn recurseWord(word: *const Word, words: *ArrayList(u8), codes: *ArrayList(ArrayList(bool)),
    running_code: *ArrayList(bool), alloc:anytype) !ArrayList(u8) {
    const actualWord = word.*;
    switch (actualWord) {
        .pure => |w| return try recursePure(&w, words, codes, running_code, alloc),
        .parent => |w| return try recurseParent(&w, words, codes, running_code, alloc),
        .source => |w| return try recurseSource(&w, words, codes, running_code, alloc),
    }
}

pub fn name(word: *const Word, alloc:anytype) !ArrayList(u8) {
    const actualWord = word.*;
    switch (actualWord) {
        .pure => |w| { 
            var array = ArrayList(u8).init(alloc);
            try array.append(w.name);
            return array;
        },
        .parent => |w| return w.name.*,
        .source => |w| return w.name.*,
    }
}

pub fn getWord(word: *const Word) !Word {
    const actualWord = word.*;
    switch (actualWord) {
        .pure => |w| return .{ .pure = w },
        .parent => |w| return .{ .parent = w },
        .source => |w| return .{ .source = w },
    }
}
const ReturnTuple = std.meta.Tuple(&.{bool, usize});
fn inArray(char: u8, array: ArrayList(u8)) ReturnTuple {
    for (array.items, 0..) |c, index| {
        if (c == char) {
            return .{
                @as(bool, true),
                @as(usize, index)
            };
        }
    }
    return .{
        @as(bool, false),
        @as(usize, 0)
    };
}

fn findMin(arr: anytype, size: usize) usize {
    var curr:f64 = arr[0];
    var curr_ind:usize = 0;
    var index:usize = 0;
    var elem:f64 = curr;
    while (index < size) : (index += 1) {
        elem = arr[index];
        if (elem < curr) {
            curr = elem;
            curr_ind = index;
        }
    }
    return curr_ind;
}

fn constructHuffman(words: ArrayList(u8), probs: *ArrayList(f64), nchars: usize, alloc: anytype) !*const SourceWord {
    var wordStructs = ArrayList(*Word).init(alloc);
    var i:usize = 0;
    while (i < nchars) : (i += 1) {
        const pureWord = try alloc.create(PureWord);
        pureWord.* = .{.name = words.items[i], .prob = probs.items[i]};
        const resultWord = try alloc.create(Word);
        resultWord.* = .{.pure = pureWord.*};
        try wordStructs.append(resultWord);
    }
    var nwords: usize = nchars;
    while (nwords > 2) {
        const minInd = findMin(probs.items, nwords);
        const minWord = wordStructs.orderedRemove(minInd);
        const minProb = probs.orderedRemove(minInd);
        const minInd2 = findMin(probs.items, nwords - 1);
        const minWord2 = wordStructs.orderedRemove(minInd2);
        const minProb2 = probs.orderedRemove(minInd2);
        const parentWord = try alloc.create(ParentWord);
        var word_names = ArrayList(u8).init(alloc);
        parentWord.* = .{.child0 = &minWord.*, .child1 = &minWord2.*, .name = &word_names};
        const resultWord = try alloc.create(Word);
        resultWord.* = .{.parent = parentWord.*};
        try wordStructs.append(resultWord);
        try probs.append(minProb + minProb2);
        nwords -= 1;
    }
    const minInd = findMin(probs.items, nwords);
    const minWord = wordStructs.orderedRemove(minInd);
    _ = probs.orderedRemove(minInd);
    const minInd2 = findMin(probs.items, nwords - 1);
    const minWord2 = wordStructs.orderedRemove(minInd2);
    _ = probs.orderedRemove(minInd2);
    const source = try alloc.create(SourceWord);
    var word_names = ArrayList(u8).init(alloc);
    source.* = .{.child0 = &minWord.*, .child1 = &minWord2.*, .name = &word_names};
    return source;
}
const NotFoundError = error{NotFound};
fn findFirst(needle: u8, words:ArrayList(u8)) !u32 {
    var i:u32 = 0;
    for (words.items) |word| {
        if (word == needle) {
            return i;
        }
        i = i + 1;
    }
    return NotFoundError.NotFound;
}

fn appendChars(buf_writer:anytype, chars:ArrayList(bool)) !void {
    for (chars.items) |char| {
        try buf_writer.writeBits(@as(u1, @intFromBool(char)), 1);
    }
    return;
}

fn appendCharCount(buf_writer:anytype, char:u8, count:u64) !void {
    try buf_writer.writeBits(char, 8);
    try buf_writer.writeBits(count, 64);
}

fn writeHeader(words:ArrayList(u8), counts:ArrayList(u64), fileLength:u64, buf_writer:anytype) !void {
    // Length of file
    try buf_writer.writeBits(@as(u64, fileLength), 64);
    // Number of characters (Unnecessary?):
    try buf_writer.writeBits(@as(u64, words.items.len), 64);
    // Character frequencies:
    for (words.items, counts.items) |word, count| {
        try appendCharCount(buf_writer, word, count);
    }
}

fn encode(reader: anytype, words:ArrayList(u8), codes:ArrayList(ArrayList(bool)), 
    buf_writer:anytype) !void {
    var buffer: [4096]u8 = undefined;
    var index:u32 = 0;
    while (true) {
        const bytes_read = try reader.read(&buffer);
        if ( bytes_read == 0 ) { 
            break;
        }
        for (buffer[0..bytes_read]) |char| {
            index = try findFirst(char, words);
            const code = codes.items[index];
            try appendChars(buf_writer, code);
        }
    }
    return;
}

fn readHeader(bit_reader:anytype, words:*ArrayList(u8), counts:*ArrayList(u64)) !u64 {
    const fileLength = try bit_reader.*.readBitsNoEof(u64, 64);
    const num_words = try bit_reader.*.readBitsNoEof(u64, 64);
    for (0..num_words) |_| {
        const char = try bit_reader.*.readBitsNoEof(u8, 8);
        const count = try bit_reader.*.readBitsNoEof(u64, 64);
        try words.append(char);
        try counts.append(count);
    }
    return fileLength;
}

fn isEqual(arr1:ArrayList(bool), arr2:ArrayList(bool)) bool {
    if (arr1.items.len != arr2.items.len) {
        return false;
    }
    for (arr1.items, arr2.items) |item1, item2| {
        if (item1 != item2) {
            return false;
        }
    }
    return true;
}
fn isPresent(code:ArrayList(bool), codes:ArrayList(ArrayList(bool))) !usize {
    for (codes.items, 0..) |prospective_code, index| {
        if (isEqual(code, prospective_code)) {
            return index;
        }
    }
    return NotFoundError.NotFound;
}

fn readData(bit_reader:anytype, out:*ArrayList(u8), _:ArrayList(ArrayList(bool)),
    _:ArrayList(u8), source:*const SourceWord, fileLength:u64, alloc:std.mem.Allocator) !void {
    var read_chars: u64 = 0;
    var current_node: Word = .{.source = source.*};
    while (read_chars < fileLength) {
        const value_raw = bit_reader.*.readBitsNoEof(u1, 1) catch |err| {
            if (err == error.EndOfStream) {
                const curr_name = try name(&current_node, alloc);
                if (curr_name.items.len != 0) {
                    std.debug.print("Some items have been discarded !!!", .{});
                    return;
                } else {
                    return;
                }
            }
            return;
        };
        const curr_name = try name(&current_node, alloc);
        if (curr_name.items.len == 1) {
            try out.append(curr_name.items[0]);
            read_chars += 1;
            current_node = .{ .source = source.* };
        }
        if (value_raw == 0) {
            const next_name = try name(&current_node.child0, alloc);
            if (next_name.items.len == 1 ) {
                try out.append(next_name.items[0]);
                read_chars += 1;
                continue;
            } else {
                current_node = .{ .parent = current_node.child0.* };
            }
        } else {
            const next_name = try name(&current_node.child1, alloc);
            if (next_name.items.len == 1 ) {
                try out.append(next_name.items[0]);
                read_chars += 1;
                continue;
            } else {
                current_node = .{ .parent = current_node.child1.* };
            }
        }
    }
    return;
}

pub fn huffmanDecode(input:[]const u8, output:[]const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const file = try std.fs.cwd().openFile(input, .{.mode = std.fs.File.OpenMode.read_only });
    defer file.close();

    const reader = file.reader();

    var bit_reader = std.io.bitReader(.little, reader);

    var chars = ArrayList(u8).init(alloc);
    var counts = ArrayList(u64).init(alloc);
    const fileLength = try readHeader(&bit_reader, &chars, &counts);
    
    var counts_sum: f64 = 0.0;
    var i: usize = 0;
    const nchars = chars.items.len;
    while (i < nchars) : (i += 1) {
        counts_sum += @as(f64, @floatFromInt( counts.items[i]));
    }
    var probs = ArrayList(f64).init(alloc);
    i = 0;
    while (i < nchars) : (i += 1) {
        try probs.append(@as(f64, @floatFromInt( counts.items[i])) / counts_sum);
    }

    const source = try constructHuffman(chars, &probs, nchars, alloc);
    var words = ArrayList(u8).init(alloc);
    var codes = ArrayList(ArrayList(bool)).init(alloc);
    var running_code = ArrayList(bool).init(alloc);
    _ = try recurseSource(source, &words, &codes, &running_code, alloc);
    var out = ArrayList(u8).init(alloc);
    try readData(&bit_reader, &out, codes, words, source, fileLength, alloc);
    
    const out_file = try std.fs.cwd().createFile(output, .{});
    defer out_file.close();

    const writer = out_file.writer();
    try writer.writeAll(out.items);
}

pub fn huffmanEncode(input:[]const u8, output:[]const u8) !void {
    const file = try std.fs.cwd().openFile(input, .{.mode = std.fs.File.OpenMode.read_only });
    defer file.close();

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    
    var chars = ArrayList(u8).init(alloc);
    var counts = ArrayList(u64).init(alloc);
    var nchars: usize = 0;

    // File reading stuff
    var fileLength: u64 = 0;
    var buffer: [8192]u8 = undefined;
    var buf_reader = std.io.bufferedReader(file.reader());
    var reader = buf_reader.reader();
    // This loop is reading "invisible" characters, take it out or not?
    while (true) {
        const bytes_read = try reader.read(&buffer);
        if ( bytes_read == 0 ) { 
            break;
        }
        for (buffer[0..bytes_read]) |char| {
            const out = inArray(char, chars);
            if (out[0]) {
                counts.items[out[1]] += @as(u64, 1);
            } else {
                nchars += 1;
                try chars.append(char);
                try counts.append(@as(u64, 1));
            }
            fileLength += 1;
        }
    }
    var counts_sum: f64 = 0.0;
    var i: usize = 0;
    while (i < nchars) : (i += 1) {
        counts_sum += @as(f64, @floatFromInt( counts.items[i]));
    }
    var probs = ArrayList(f64).init(alloc);
    i = 0;
    while (i < nchars) : (i += 1) {
        try probs.append(@as(f64, @floatFromInt( counts.items[i])) / counts_sum);
    }
    const source = try constructHuffman(chars, &probs, nchars, alloc);
    var words = ArrayList(u8).init(alloc);
    var codes = ArrayList(ArrayList(bool)).init(alloc);
    var running_code = ArrayList(bool).init(alloc);
    _ = try recurseSource(source, &words, &codes, &running_code, alloc);
    // std.log.debug("{s}", .{source.*.name.items});
    // std.log.debug("words: {any}\n", .{words});
    // for (words.items, codes.items) |word, code| {
    //     std.log.debug("Word: {c} => {any}\n", .{word, code.items});
    // }
    const out_file = try std.fs.cwd().createFile(output, .{});
    defer out_file.close();

    var buf_writer = std.io.bufferedWriter(out_file.writer());
    const writer = buf_writer.writer();
    var bit_writer = std.io.bitWriter(.little, writer);
    try writeHeader(chars, counts, fileLength, &bit_writer);
    // seek file to start
    try file.seekTo(0);
    try encode(&reader, words, codes, &bit_writer);
    try bit_writer.flushBits();
    try buf_writer.flush();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const firstarg = args[1];
    if (std.mem.eql(u8, firstarg, "--test")) {
        try huffmanEncode("foo.txt", "foo-comp.huff");
        try huffmanDecode("foo-comp.huff", "foo-decomp.txt");
    } else if (std.mem.eql(u8, firstarg, "--compress")) {
        if (args.len < 4) {
            std.debug.print("Not enough parameters for compression: ./main --compress <input> <output>", .{});
            return;
        }
        const input = args[2];
        const output = args[3];
        try huffmanEncode(input, output);
    } else if (std.mem.eql(u8, firstarg, "--decompress")) {
        if (args.len < 4) {
            std.debug.print("Not enough parameters for decompression: ./main --compress <input> <output>", .{});
            return;
        }
        const input = args[2];
        const output = args[3];
        try huffmanDecode(input, output);
    } else {
        std.debug.print("Unrecognized option: {s}", .{firstarg});
    }
}
