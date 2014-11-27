#!/usr/bin/perl

# The MIT License (MIT)
#
# Copyright (c) 2014 Tim Wilkens
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

use strict;
use warnings;

use Getopt::Long;

my $banner = <<EOF;

  ______        _    _             _   
 |  ____|      | |  | |           | |  
 | |__ _____  _| |__| |_   _ _ __ | |_ 
 |  __/ _ \\ \\/ /  __  | | | | '_ \\| __|
 | | | (_) >  <| |  | | |_| | | | | |_ 
 |_|  \\___/_/\\_\\_|  |_|\\__,_|_| |_|\\__|
                                       

EOF

my %work;
my %seen;

my $bullet = "\"<<>";

my ($prey, $proxy, $proxy_port);
GetOptions(
  "prey=s" => \$prey,
  "proxy=s" => \$proxy,
  "port=s" => \$proxy_port,
);

if (!$prey) {
  die "Must provide --prey url.\n";
}

print $banner;

if ($prey !~ /^https?:\/\//) {
  $prey = ("http://" . $prey);
}

$work{$prey} = 1;

my $hound = Hound->new();
if ($proxy) {
  if ($proxy_port) {
    print "Proxy: $proxy:$proxy_port\n\n";
    $hound->set_proxy("http://$proxy:$proxy_port/");
  } else {
    die "Must provide --proxy AND --port.\n";
  }
}

while (my $fox = get_work()) {
  sleep(1);
  if ($hound->smells_fox($fox)) {
    add_work($hound->get_expanded_links());
    if ($hound->killed_fox()) {
      print "\033[31m$fox\033[0m\n";
      next;
    }
  }
  print "$fox\n";
}

print "No more game. Exiting.\n";

sub add_work {
  my @links = @_;
  for my $l (@links) {
    if (!$seen{$l}) {
      $work{$l} = 1;
    }
  }
}

sub get_work {
  if (keys %work) {
    my $url = (keys %work)[0];
    delete $work{$url};
    $seen{$url} = 1;
    return $url;
  } else {
    return undef;
  }
}


BEGIN {

package Hound;

use strict;
use warnings;

use WWW::Mechanize;

my @AGENTS = WWW::Mechanize::known_agent_aliases();

sub new {
  my $class = shift;
  my $self = bless {}, $class;
  $self->{nose} = WWW::Mechanize->new();
  $self->{nose}->add_header(Referer => undef);
  $self->{nose}->max_redirect(0);
  return $self;
}

sub get_expanded_links {
  my $self = shift;
  my @links = $self->{nose}->links();
  my $base = $self->{nose}->uri();
  $base =~ s/\/$//;
  @links = map { if ($_ =~ /^\//) { $base . "/$_" } else { $_ } }  @links;
  return map  { $_ =~ s/(?<!:)\/+/\//g; $_ }
         grep { $_ !~ /\.(png|jpg|gif|css)/ }
         grep { $_ =~ /^http/ }
         grep { $_ !~ /^\// }
         map  { $_->url() } @links;
}

sub smells_fox {
  my ($self, $url) = @_;
  $self->update_useragent();
  eval { $self->{nose}->get($url); };
  if ($@ || $self->{nose}->status() != 200) {
    return 0;
  }
  return 1;
}

sub update_useragent {
  my $self = shift;
  $self->{nose}->agent_alias($AGENTS[rand(@AGENTS)]);
}

sub killed_fox {
  my $self = shift;
  return 0 unless $self->fox_in_sight();
  my $fox = $self->{nose}->uri();
  $fox = ($fox =~ /\/$/) ? "$fox" . "$bullet/" : "$fox/" . "$bullet/";
  if ($self->smells_fox($fox)) {
    return $self->fox_is_dead();
  } else {
    return 0;
  }
}

sub fox_in_sight {
  my $self = shift;
  my $content = $self->{nose}->content(decoded_by_headers => 1);
  my $u = $self->{nose}->uri();
  return ($content =~ /$u/) ? 1 : 0;
}

sub fox_is_dead {
  my $self = shift;
  my $content = $self->{nose}->content(decoded_by_headers => 1);
  return ($content =~ /(?<!\\)$bullet/) ? 1 : 0;
}


sub set_proxy {
  my $self = shift;
  $self->{nose}->proxy(['http', 'https'], "http://$proxy:$proxy_port/");
}

}
