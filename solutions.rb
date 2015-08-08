require 'json'
require 'time'

$last_update = Time.new(0)
def refresh_sols
  $solutions_list = JSON.parse(`curl --user ':8+FseeDc/XZMkhiIMvA2y5xNMSCoc+DA4XWeTe/2Xz0=' \
    https://davar.icfpcontest.org/teams/86/solutions`)

  $solutions_list.each_with_index do |sol, i|
    $solutions_list[i]['createdAt'] = Time.parse(sol['createdAt']).localtime
  end

  $solutions = $solutions_list.group_by { |s| s['problemId'] }
  $solutions.each do |_, sols|
    sols.sort! { |sol1, sol2| sol2['createdAt'] <=> sol1['createdAt'] }
  end

  $last_update = Time.now
end

def print_sol(sol)
  sol_cmds = sol['solution']
  puts "#{sol['createdAt']} (seed: #{sol['seed']}) - #{sol['score'] || '-'} (pw #{sol['powerScore'] || '-'})"
  puts "  -- #{sol_cmds.length <= 70 ? sol_cmds : sol_cmds[0, 70] + '...'}"
end

def print_problem_status(prob_id, force_cache = false)
  refresh_sols if force_cache || Time.now > $last_update + 2
  $solutions[prob_id].take(20).each { |s| print_sol(s) }
  nil
end

# Usage in REPL: print_problem_status(<prob_id>)
