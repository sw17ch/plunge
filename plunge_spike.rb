#!/usr/bin/env ruby

begin
    require 'Win32/Console/ANSI' if RUBY_PLATFORM =~ /win32|mingw/
rescue LoadError
    raise 'You must gem install win32console to use color on Windows'
end

require 'coderay'
require 'paint'

Paint.mode = 256

def mk_section(file, start)
  {
    file: file,
    start: start,
    end: start,
    lines: []
  }
end

$colors = [ :red, :green, :yellow, :blue, :magenta, :cyan ]

def next_term_color
  $colors.push(color = $colors.shift)
  return color
end

FILE = ARGV[0]
original = File.read(FILE)
preprocessed = `gcc -E #{FILE}`

directive_pat = /^# (?<line>\d+) "(?<file>[^"]+)" ?(?<flags>(?:\d+ ?)*)$/

sections = [{file: "", start: 0, end: 0, lines: []}]
current_file = FILE
reference_line = 1

preprocessed.lines.each do |line|
  if m = line.match(directive_pat)
    if m[:flags].match(/1/) # entering new file
      if FILE == current_file # at the top
        sections << mk_section(m[:file], reference_line)
      end
      current_file = m[:file]

    elsif m[:flags].match(/2/) # returning to previous file
      current_file = m[:file]

      if FILE == current_file # at the top
        reference_line = m[:line].to_i
        sections.last[:end] = m[:line].to_i
        sections << mk_section(m[:file], m[:line].to_i)
      end
    end
  else
    reference_line += 1
    sections.last[:lines] << line.chomp
    sections.last[:end] = reference_line
  end
end

longest_ori_line = original.lines.map {|l| l.length}.max
longest_cpp_line = sections.map {|s| s[:lines].map {|l| l.length}}.flatten.max

ori_fmt = "%-#{longest_ori_line}s"
cpp_fmt = "%-#{longest_cpp_line}s"

fmt = "| #{ori_fmt} ||| #{cpp_fmt} |"
hdr = (fmt % ["Original", "Preprocessed"])
sep = "=" * hdr.length

puts hdr
puts sep
sections.each do |s|
  next if s[:lines].length == 0

  color = next_term_color

  ori_lines = original.lines.to_a[((s[:start] - 1)...(s[:end] - 1))]
  cpp_lines = s[:lines]

  [ori_lines.length, cpp_lines.length].max.times do |i|
    ori_line = ori_fmt % (ori_lines[i] || '.' * longest_ori_line).to_s.chomp
    cpp_line = cpp_fmt % (cpp_lines[i] || '.' * longest_cpp_line).to_s.chomp


    print Paint["| ", color]
    print CodeRay.scan(ori_line, :c).term
    print Paint[" ||| ", color]
    print CodeRay.scan(cpp_line, :c).term
    print Paint[" |\n", color]
  end
end
