/*
  Copyright 2012 Lucanus Simonson
 
  Use, modification and distribution are subject to the Boost Software License,
  Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
  http://www.boost.org/LICENSE_1_0.txt).
*/
#ifndef BOOST_POLYGON_SKELETON_DETAIL_HPP
#define BOOST_POLYGON_SKELETON_DETAIL_HPP
#include <boost/polygon/point_concept.hpp>
#include <boost/polygon/segment_concept.hpp>
#include <boost/polygon/polygon_set_concept.hpp>
#include <boost/polygon/segment_utils.hpp>
#include <queue>
#include "skeleton_predicates.hpp"

namespace boost { namespace polygon { 
#ifdef BOOST_POLYGON_DEBUG1
    inline void debug1(const char* msg) {std::cout << std::string(msg) << std::endl;}
#else
    inline void debug1(const char* msg) {}
#endif


    template <typename PointType, typename Segment2, typename Segment3>
    bool
    //computes in point the intersection of the lines of Segment2 and Segment3
    //if Segment2 and Segment3 are parallel and non-colinear it returns false
    //if segment2 and segment3 are parallel and colinear it returns the least low point in the direction
    //both segments are directed, or if opposite directions it returns the midpoint between their two low points
    generalized_intersection(PointType& point, const Segment2& segment2, const Segment3& segment3) {
      typedef typename point_traits<PointType>::coordinate_type UnitOut;
      UnitOut x11, x21, x12, x22, y12, y22, y11, y21, x_num, x_den, y_num, y_den, dx1, dy1, dx2, dy2;
      x11 = UnitOut(segment2.low().get(HORIZONTAL));
      x21 = UnitOut(segment3.low().get(HORIZONTAL));
      y11 = UnitOut(segment2.low().get(VERTICAL));
      y21 = UnitOut(segment3.low().get(VERTICAL));
      x12 = UnitOut(segment2.high().get(HORIZONTAL));
      x22 = UnitOut(segment3.high().get(HORIZONTAL));
      y12 = UnitOut(segment2.high().get(VERTICAL));
      y22 = UnitOut(segment3.high().get(VERTICAL));
      dx1 = x12 - x11;
      dy1 = y12 - y11;
      dx2 = x22 - x21;
      dy2 = y22 - y21;
      if(dx1 * dy2 - dx2 * dy1 == 0) {
        if((x22 - x11) * dx2 == (y22 - y11) * dy2)
          return false; //lines have same slope but not colinear
        UnitOut dll = (x11 - x21) * (x11 - x21) + (y11 - y21) * (y11 - y21);
        UnitOut dlh = (x11 - x22) * (x11 - x22) + (y11 - y22) * (y11 - y22);
        UnitOut dhl = (x12 - x21) * (x12 - x21) + (y12 - y21) * (y12 - y21);
        UnitOut dhh = (x12 - x22) * (x12 - x22) + (y12 - y22) * (y12 - y22);
        if(dhh < dhl && dhh < dlh && dhh < dll) {
          //segments are directed toward eachother
          point = PointType((x12+x22)/2,(y12+y22)/2); //average of high points
        } else {
          //away or in the same direction
          point = PointType((x11+x21)/2,(y11+y21)/2); //average of low points
        }
      }
      x_num = (x11 * dy1 * dx2 - x21 * dy2 * dx1 + y21 * dx1 * dx2 - y11 * dx1 * dx2);
      x_den = (dy1 * dx2 - dy2 * dx1);
      y_num = (y11 * dx1 * dy2 - y21 * dx2 * dy1 + x21 * dy1 * dy2 - x11 * dy1 * dy2);
      y_den = (dx1 * dy2 - dx2 * dy1);
      UnitOut x = x_num / x_den;
      UnitOut y = y_num / y_den;
      point = PointType(x, y);
      std::cout << x11 << "," << y11 << "->" << x12 << "," << y12 << " " << x21 << "," << y21 << "->" << x22 << "," << y22 << "\n";
      std::cout << "Intersection Point " << x << " " << y << std::endl;
      return true;
    }


    template <typename Segment1, typename Segment2, typename Segment3>
      bool
    //computes in segment1 a segment with low point at the intersection of the lines
    //of segment2 and segment3 oriented in the direction of the angle bisector of segment2
    // and segment3
    //degenerate case where segments are parallel and colinear
    bisector(Segment1& segment1, const Segment2& segment2, const Segment3& segment3) {
      typedef typename segment_traits<Segment1>::coordinate_type UnitOut;
      point_data<UnitOut> gint;
      if(generalized_intersection(gint, segment2, segment3)) {
        UnitOut x11, x21, x12, x22, y12, y22, y11, y21, dx1, dy1, dx2, dy2;
        segment1.low(gint);
        x11 = UnitOut(segment2.low().get(HORIZONTAL));
        x21 = UnitOut(segment3.low().get(HORIZONTAL));
        y11 = UnitOut(segment2.low().get(VERTICAL));
        y21 = UnitOut(segment3.low().get(VERTICAL));
        x12 = UnitOut(segment2.high().get(HORIZONTAL));
        x22 = UnitOut(segment3.high().get(HORIZONTAL));
        y12 = UnitOut(segment2.high().get(VERTICAL));
        y22 = UnitOut(segment3.high().get(VERTICAL));
        dx1 = x12 - x11;
        dy1 = y12 - y11;
        dx2 = x22 - x21;
        dy2 = y22 - y21;
        
        point_data<UnitOut> p1(x11, y11);
        point_data<UnitOut> p2(x21, y21);
        if(p1 == gint)
          p1 = point_data<UnitOut>(x12, y12);
        if(p2 == gint)
          p2 = point_data<UnitOut>(x22, y22);
        UnitOut dist1 = euclidean_distance(gint, p1);
        UnitOut dist2 = euclidean_distance(gint, p2);
        std::cout << dist1 << " " << dist2 << "\n";
        UnitOut x_out = gint.x() + ((p1.x() - gint.x())*dist2/dist1 + (p2.x()-gint.x()))/2;
        UnitOut y_out = gint.y() + ((p1.y() - gint.y())*dist2/dist1 + (p2.y()-gint.y()))/2;
        point_data<UnitOut> end_point(x_out, y_out);
        if(orientation(segment2, end_point) < 0) {
          x_out = gint.x() - ((p1.x() - gint.x())*dist2/dist1 + (p2.x()-gint.x()))/2;
          y_out = gint.y() - ((p1.y() - gint.y())*dist2/dist1 + (p2.y()-gint.y()))/2;
          end_point = point_data<UnitOut>(x_out, y_out);
        }
        std::cout << x_out << " " << y_out << "\n";
        if(orientation(segment2, end_point) != orientation(segment3, end_point)) { 
          std::cout << "we want the other bisector in this case\n";
          y_out = gint.y() + ((p1.x() - gint.x())*dist2/dist1 + (p2.x()-gint.x()))/2;
          x_out = gint.x() - ((p1.y() - gint.y())*dist2/dist1 + (p2.y()-gint.y()))/2;
          end_point = point_data<UnitOut>(x_out, y_out);
          std::cout << x_out << " " << y_out << "\n";
          std::cout << segment2.low().x() << " " << segment2.low().y() << " " << 
            segment2.high().x() << " " << segment2.high().y() << " " << std::endl;
          if(orientation(segment2, end_point) < 0) {
            y_out = gint.y() - ((p1.x() - gint.x())*dist2/dist1 + (p2.x()-gint.x()))/2;
            x_out = gint.x() + ((p1.y() - gint.y())*dist2/dist1 + (p2.y()-gint.y()))/2;
            end_point = point_data<UnitOut>(x_out, y_out);
            std::cout << x_out << " " << y_out << "\n";
          }
        }
        end_point = point_data<UnitOut>(x_out, y_out);
        segment1.high(end_point);
        return true;
      }
      return false;
    }

    template <typename coordinate_type, typename output_coordinate_type>
    struct skeleton_detail {

      //DATA STRUCTURES
      typedef segment_data<output_coordinate_type> edge_type;
      typedef point_data<output_coordinate_type> vertex_type;
      typedef segment_data<output_coordinate_type> segment_type;
      
      //forward declare
      struct future_intersection;
      struct skeleton_node;

      //EVENT OBJECT can represent either split event or edge event with same type
      struct event {
        point_data<output_coordinate_type> point;
        output_coordinate_type distance; //use for lazy compare, exact compare can be done with original coordinates in *parent
        //event consists of intersection between angle bisectors of parent and parent->next or parent and split_on_next 
        future_intersection* parent;
        future_intersection* parent2;
        //refer back to the edge between the target and its next
        future_intersection* split_on_next; //if null is an edge event
        
        bool operator<(const event& that) const {
          debug1("event::operator<");
          return distance < that.distance; 
        }
        bool operator>(const event& that) const {
          debug1("event::operator>");
          return distance > that.distance; 
        }

        output_coordinate_type x() const { return point.x(); }
        void x(const output_coordinate_type& val) { point.x(val); }
        
        output_coordinate_type y() const { return point.y(); }
        void y(const output_coordinate_type& val) { point.y(val); }
        
        output_coordinate_type r() const { return distance; }
        void r(const output_coordinate_type& val) { distance = val; }
      };
      typedef typename boost::polygon::detail::skeleton_predicates
      <typename boost::polygon::detail::skeleton_ctype_traits<coordinate_type> >::template event_formation_predicate<
        segment_data<coordinate_type>, event> create_event;

      //SET OF CIRCULAR LINKED LISTS OF FUTURE INTERSECTIONS
      // each future intersection is associated with a pair of edges of the polygon
      // originally coming from edges incident on each polygon vertex
      // polygon holes are circular sub lists of each list such that split events can be found
      struct future_intersection {
        edge_type edge; //the edge that is next between this future intersection and the one stored in *next
        future_intersection* prev;
        future_intersection* next;
        future_intersection* next_split_edge;
        skeleton_node* parent_node;
        output_coordinate_type active_distance;
        //split events that refer to the edge between this and next
        std::vector<typename std::set<event>::iterator> active_splits_on_next;
        std::string label;
        bool is_dead;
        future_intersection() : prev(0x0), next(0x0), next_split_edge(0x0), 
                                parent_node(0x0), is_dead(false), active_distance(-1.0) {
          static char local_label = 'a';
          label = local_label;
          if(local_label == 'z')
            local_label = 'a';
          else
            ++local_label;
        }
        ~future_intersection() {
          debug1("~future_intersection()");
          prev = 0x0;
          next = 0x0;
          parent_node = 0x0;
        }
      };

      static void delete_future_intersection_ring(future_intersection* head) {
        debug1("delete_future_intersection_ring()");
        future_intersection* current = head;
        if(head != 0x0)
          if(head->prev != 0x0)
            head->prev->next = 0x0; //give us a stopping point
        while(current != 0x0) {
          if(current->prev != 0x0)
            current->prev->next = 0x0;
          future_intersection* dead_node = current;
          current = current->next;
          delete dead_node;
        }
      }

      //EVENT QUEUE of event objects by minimum weighted distance from edges
      struct event_priority {
        bool operator()(const event& e1, const event& e2) {
          return e1 > e2;
        }
      };
      typedef std::priority_queue<event, std::vector<event>, event_priority> event_queue_type;

      //SKELETON OUTPUT DATA STRUCTURE
      //OUTPUT REQUIREMENTS
      //provide floating point representation of skeleton nodes and also references to all the input polygon edges from which 
      //the node is weighted equidistant, that way original coordinates can be used by algorithms consuming skeleton data
      //handle the case where there are more than three edges that are weighted cocirular by a zero length skeleton segment
      //that can be collapsed in post processing
      
      struct skeleton_node {
        skeleton_node(const point_data<output_coordinate_type>& pt) {
          debug1("skeleton_node()");
          static char local_label = 'A';
          label = local_label;
          if(local_label == 'Z')
            local_label = 'A';
          else
            ++local_label;
          vertex = pt;
          linked_nodes[0] = linked_nodes[1] = linked_nodes[2] = 0x0;
        }
        void add_edge(skeleton_node* link) {
          for(int i = 0; i < 3; ++i) {
            if(!linked_nodes[i]) {
              linked_nodes[i] = link;
              break;
            }
          }
        }
        std::string label;
        point_data<output_coordinate_type> vertex;
        skeleton_node* linked_nodes[3]; //three is the common case
        //for higher order nodes insert artificial zero length skeleton edges and duplicate node coordinates
        //to create a 3-ary tree
      };

      //static member functions

      static void find_split_event(future_intersection* first, future_intersection* second, event_queue_type& event_queue) {
        if(orientation(first->edge, second->edge) < 1) {
          //std::cout << "initialize reflex vertex\n";
          future_intersection* inner = second->next->next;
          while(inner != second) {
            event e2;
            if(compute_split_event(e2, second, inner)) {
              if(e2.distance <= e2.parent->active_distance || e2.parent->active_distance < 0) {
                //split event has no parent2
                std::cout << "inserting split event\n";
                event_queue.push(e2);
                e2.parent->active_distance = e2.distance;
              }
            }
            inner = inner->next;
          }
        }
      }

      //ORIGINAL POLYGON EDGES
      //simply copy the original polygon edges into the future intersection edge pair data structure
      static void initialize_skeleton_edges(std::vector<skeleton_node*>& skeleton, 
                                            std::vector<future_intersection*>& all_faces,
                                            event_queue_type& event_queue,
                                            const std::vector<edge_type>& edges) {
        debug1("initialize_skeleton_edges");
        if(edges.empty()) return;
        rectangle_data<output_coordinate_type> domain;
        envelope_segments(domain, edges.begin(), edges.end());
        future_intersection* head = 0x0, *current = 0x0, *prev = 0x0;
        for(std::size_t i = 0; i < edges.size(); ++i) {
          skeleton.push_back(new skeleton_node(point_data<output_coordinate_type>
                                               (output_coordinate_type(edges[i].low().get(HORIZONTAL)),
                                                output_coordinate_type(edges[i].low().get(VERTICAL)))));
          current = new future_intersection();
          all_faces.push_back(current);
          if(i == 0) head = current;
          current->edge = edges[i];
          current->parent_node = (skeleton.back());
          current->prev = prev;
          current->is_dead = false;
          if(prev) {
            prev->next = current;
          }
          prev = current;
        }
        current->next = head;
        head->prev = current;

        current = head;
        do {
          debug1("loop");
          find_split_event(current->prev, current, event_queue);
          event e;
          if(compute_edge_event(e, current, current->next, domain)) {
            if((e.distance <= e.parent->active_distance || e.parent->active_distance < 0) &&
               (e.distance <= e.parent2->active_distance || e.parent2->active_distance < 0)) {
              event_queue.push(e);
              e.parent->active_distance = e.distance;
              e.parent2->active_distance = e.distance;
            }
          }
          current = current->next;
        } while(current != head);
      }

      template<typename polygon_data_type>
      static void initialize_skeleton_single_polygon(std::vector<skeleton_node*>& skeleton, 
                                                     std::vector<future_intersection*>& all_faces,
                                                     event_queue_type& event_queue,
                                                     const polygon_data_type& polygon,
                                                     bool reverse_winding = false) {
        debug1("initialize_skeleton_single_polygon");
        //polygons may be open or closed
        if(polygon.size() == 0) return;
        std::vector<edge_type> edges;
        typename polygon_data_type::iterator_type itr = polygon.begin(), first, prev;
        first = itr;
        prev = itr;
        for(++itr; itr != polygon.end(); ++itr) {
          //ignore duplicate points
          if(*prev != *itr) {
            if(reverse_winding)
              edges.push_back(edge_type(*itr, *prev));
            else
              edges.push_back(edge_type(*prev, *itr));
            prev = itr;
          }
        }
        if(*prev != *first){
          if(reverse_winding)
            edges.push_back(edge_type(*first, *prev));
          else
            edges.push_back(edge_type(*prev, *first));
        }
        if(reverse_winding) {
          std::reverse(edges.begin(), edges.end());
        }
        initialize_skeleton_edges(skeleton, all_faces, event_queue, edges);
      }

      template<typename geometry_type>
      static void initialize_skeleton(std::vector<skeleton_node*>& skeleton, std::vector<future_intersection*>& all_faces,
                                      event_queue_type& event_queue,
                                      const geometry_type& polygon_set) {
        debug1("initialize_skeleton");
        std::vector<polygon_with_holes_data<coordinate_type> > polys;
        assign(polys, polygon_set); //this makes winding correct for polygons and holes
        for(std::size_t i = 0; i < polys.size(); ++i) {
          initialize_skeleton_single_polygon(skeleton, all_faces, event_queue, polys[i]);
          for(typename polygon_with_holes_data<coordinate_type>::iterator_holes_type itrh = polys[i].begin_holes();
              itrh != polys[i].end_holes(); ++itrh) {
            initialize_skeleton_single_polygon(skeleton, all_faces, event_queue, *itrh);
          }
        }
      }


      //COMPUTE EDGE EVENT COORDINATES
      //given four general line segments find the intersection between their angle bisectors or return false if it does not exist
      //robustness concern: if two line segments don't share a vertex the bisector line will be anchored on their 
      //projected intersection, which is a rational quantity, use the original coordinates of the line segments in the formula
      //to compute the angle bisectors intersection rather than the anchor point of the bisector for computation
      //perform quick predicates to check if the two bisectors should ever intersect
      //include weight on each edge to bias the angle bisector
      //include bound on intersection weighted distance 
      //required degenerate case, if they are colinear then the bisector is perpendicular at their mid point between them on the line 
      //(shared coordinate if abutting) if they are at 180 degrees
      //optional degenerate case, if they are colinear but at zero degrees (spike) then the bisector is at the lesser of their 
      //two start points and parallel to the segments

      template <typename Segment>
      static void print_segment(const Segment& s) {
        std::cout << s.low().x() << "," << s.low().y() << "->" << s.high().x() << "," << s.high().y() << "\n";
      }

      static bool compute_split_event_point(event& e, edge_type e1, edge_type e2, edge_type e3, edge_type e4, edge_type e5) {
        debug1("compute_split_event_point");
        segment_type segment1, segment2, segment3, segment4, segment5;
        print_segment(e1);
        print_segment(e2);
        print_segment(e4);
        point_data<output_coordinate_type> v;
        //compute that the intersection of the lines of e1 and e2 are on the correct side of the "opposite" segment e4
        if(!generalized_intersection(v, e1, e2)) {
          debug1("no intersection");
          return false;
        }
        if(orientation(e4, v) <= 0) { //check that reflex vertex ray originates on the correct side of "opposite" edge e4
          debug1("wrong side opposite");
          return false;
        }
        //compute the bisector of e1 and e2, this is the reflex vertex bisector
        if(bisector(segment2, e3, e4)) {
          debug1("has bisector2");
          if(bisector(segment3, e4, e5)) {
            debug1("has bisector3");
            segment_data<coordinate_type> s1, s2, s3;
            assign(s1, e1);
            assign(s2, e2);
            assign(s3, e4);
            if(!create_event()(s1, s2, s3, &e)) {
              return false;
            }
            
            point_data<output_coordinate_type> point = e.point;
            std::cout << point.x() << " " << point.y() << std::endl;
            //test that point is on the coorect side of e1 and e2 and e4
            if(orientation(e1, point) <= 0)
              return false;
            if(orientation(e2, point) <= 0)
              return false;
            if(orientation(e3, point) <= 0)
              return false;
            if(orientation(e4, point) <= 0)
              return false;
            if(orientation(e5, point) <= 0)
              return false;
            debug1("passed simple orientation tests");
              //test that the point is on the correct side of the besector of e3,e4 and e4,e5
              //it should be clockwise the bisector of e3,e4 and counterclockwise the bisector of e4,e5
              //if e3,e4/e4e5 are concave then the correct sides will be reversed
            print_segment(e3);
            print_segment(e4);
            //print_segment(segment2);
            if((distance_squared_to_line(point, e4) - distance_squared_to_line(point, e3)) > 1e-6)
              return false;
            // std::cout << orientation(e3, e4) << " " << orientation(segment2, point) << std::endl;
            // if(orientation(e3, e4) == orientation(segment2, point))
            //   return false;
            debug1("passed complex orientation test1");
            print_segment(e4);
            print_segment(e5);
            print_segment(segment3);
            std::cout << distance_squared_to_line(point, e4) << " " << distance_squared_to_line(point, e5) << "\n";
            std::cout << (distance_squared_to_line(point, e4) > distance_squared_to_line(point, e5)) << "\n";
            std::cout << (distance_squared_to_line(point, e4) - distance_squared_to_line(point, e5)) << std::endl;
            std::cout << (distance_squared_to_line(point, e4) - distance_squared_to_line(point, e5) > 1e-6) << std::endl;
            if((distance_squared_to_line(point, e4) - distance_squared_to_line(point, e5)) > 1e-6)
              return false;
            //if(orientation(e4, e5) != orientation(segment3, point))
            //  return false;
            debug1("passed complex orientation test2");
            //we seem to have a good split event point
            return true;
          }
        }
        return false;
      }

      template <typename SegmentType>
      static output_coordinate_type distance_squared_to_line(const point_data<output_coordinate_type>& pt, const SegmentType& segment) {
        //(ax+by+c)/sqrt(a^2+b^2)
        //(ax+by+c)(ax+by+c)/(a^2+b^2)
        output_coordinate_type x11, x12, y12, y11, dx1, dy1, slope;
        x11 = output_coordinate_type(segment.low().get(HORIZONTAL));
        y11 = output_coordinate_type(segment.low().get(VERTICAL));
        x12 = output_coordinate_type(segment.high().get(HORIZONTAL));
        y12 = output_coordinate_type(segment.high().get(VERTICAL));
        dx1 = x12 - x11;
        dy1 = y12 - y11;
        if(dx1 == 0.0)
          return (pt.x() - x11)*(pt.x() - x11);
        slope = dy1/dx1;
        //0 = -slope * x + y + slope * x11 - y11
        output_coordinate_type a = -slope;
        // b = 1;
        output_coordinate_type c = slope * x11 - y11;
        output_coordinate_type numf = a*pt.x() + pt.y() + c;
        return numf*numf/(a*a + 1);
      }

      static bool compute_split_event(event& e, future_intersection* first, future_intersection* opposite) {
        debug1("compute_split_event");
        point_data<output_coordinate_type> pt;
        if(compute_split_event_point(e, first->prev->edge, first->edge, opposite->prev->edge, opposite->edge, opposite->next->edge)) {
          std::cout << "found split event point\n";
          e.parent = first;
          e.parent2 = first->prev;
          e.split_on_next = opposite;
          return true;
        }
        return false;
      }

      static bool compute_edge_event_point(event& e, 
                                           edge_type e1, edge_type e2, edge_type e3, edge_type e4) {
        segment_data<coordinate_type> s1, s2, s3;
        assign(s1, e1);
        assign(s2, e2);
        assign(s3, e4);
        return create_event()(s1, s2, s3, &e);
      }

      static bool compute_edge_event(event& e, future_intersection* first, future_intersection* second, 
                                     const rectangle_data<output_coordinate_type>& domain) {
        point_data<output_coordinate_type> pt;
        if(compute_edge_event_point(e, first->prev->edge, first->edge, second->prev->edge, second->edge)) {
          debug1("domain");
          point_data<output_coordinate_type> pt = e.point;
          std::cout << pt.x() << " " << pt.y() << std::endl;
          //edge event points outside (or on the boundary of) the polygon bounding box are useless and not worth processing
          //they would never be reached by the forward progress of the algorithm through the event queue
          //and would just slow it down and waste space
          if(contains(domain, pt, false)) {
            //compute distance from point to edge
            segment_type s1;
            assign(s1, first->edge);
            e.parent = first;
            e.parent2 = second;
            e.split_on_next = 0x0;
            return true;
          }
        }
        return false;
      }
      
      

      //COMPUTE SPLIT EVENT COORDINATES
      //given three general line segments find the intersection between any two angle bisectors of the three segments
      //if two of the three segments are parallel choose the other angle bisector
      //if all three are parallel (two should be colinear) find the point half way between the bisector of the colinear points
      //the line of the third segment
      
      //TODO

      static void find_edge_event(future_intersection* i1, future_intersection* i2,
                                  event_queue_type& event_queue, const rectangle_data<output_coordinate_type>& domain) 
      {
        event new_event;
        if(compute_edge_event(new_event, i1, i2, domain)) {
          std::cout << "new event " << new_event.point.x() << " " << new_event.point.y() << std::endl;
          if(orientation(i1->prev->edge, new_event.point) > 0 &&
             orientation(i1->edge, new_event.point) > 0 &&
             orientation(i2->edge, new_event.point) > 0) {
            if((new_event.distance <= new_event.parent->active_distance || new_event.parent->active_distance < 0) &&
               (new_event.distance <= new_event.parent->next->active_distance || new_event.parent->next->active_distance < 0)) {
              event_queue.push(new_event);
              new_event.parent->active_distance = new_event.distance;
              new_event.parent->next->active_distance = new_event.distance;
            } else {
              std::cout << "distance test rejection " << new_event.parent->active_distance << "\n";
            }
          } else {
            std::cout << "orientation test rejection\n";
          }
        }
      }

      static void print_ring(future_intersection* fi) {
        future_intersection* current = fi;
        do {
          std::cout << current->label << "->";
          current = current->next;
        } while(current != fi);
        std::cout << fi->label << std::endl;
        do {
          std::cout << current->label << "->";
          current = current->prev;
        } while(current != fi);
        std::cout << fi->label << std::endl;
      }

      static void process_event(event current_event, std::vector<skeleton_node*>& skeleton, std::vector<future_intersection*>& all_faces,
                                event_queue_type& event_queue, const rectangle_data<output_coordinate_type>& domain) {
        if(current_event.split_on_next) {
          //split_event
          
          std::cout << "split event " << current_event.point.x() << " " << current_event.point.y() << std::endl;
          print_ring(current_event.parent);
          //if the split_on_next edge was previously split it will have a circular linked list of potential
          //future intersections that we need to search for the right one
          bool found = false;
          if(current_event.split_on_next->next_split_edge) {
            future_intersection* current = current_event.split_on_next;
            do{
              std::cout << current << std::endl;
              if(!current->is_dead) {
                debug1("is alive");
                event e2;
                if(compute_split_event(e2, current_event.parent, current)) {
                  debug1("found correct split edge");
                  current_event.split_on_next = current;
                  found = true;
                  break;
                }
              }
              current = current->next_split_edge;
            } while (current != current_event.split_on_next);
          } else {
            found = true;
          }
          if(!found)
            return;
          //this is the dual split event of a pseudo split
          if(current_event.parent->prev != current_event.parent2)
            return;
          //check for pseudo split
          std::cout << distance_squared_to_line(current_event.point, current_event.split_on_next->edge) << std::endl;
          std::cout << distance_squared_to_line(current_event.point, current_event.split_on_next->next->edge) << std::endl;

          if(abs(distance_squared_to_line(current_event.point, current_event.split_on_next->edge) -
                 distance_squared_to_line(current_event.point, current_event.split_on_next->next->edge)) < 1e-6) {
            std::cout << "pseudo split 1\n";

            skeleton.push_back(new skeleton_node(current_event.point));
            current_event.parent->parent_node->add_edge(skeleton.back());
            std::cout << current_event.parent->parent_node->label << "->" << skeleton.back()->label << std::endl;
            skeleton.back()->add_edge(current_event.parent->parent_node);
            current_event.parent->parent_node = skeleton.back();
            
            skeleton.push_back(new skeleton_node(current_event.point));
            current_event.split_on_next->next->parent_node->add_edge(skeleton.back());
            std::cout << current_event.split_on_next->next->parent_node->label << "->" << skeleton.back()->label << std::endl;
            skeleton.back()->add_edge(current_event.split_on_next->next->parent_node);
            current_event.split_on_next->next->parent_node = skeleton.back();

            skeleton.back()->add_edge(skeleton[skeleton.size()-2]);
            skeleton[skeleton.size()-2]->add_edge(skeleton.back());

            current_event.parent->prev = current_event.split_on_next;
            current_event.parent2->next = current_event.split_on_next->next;
            current_event.split_on_next->next->prev = current_event.parent2;
            current_event.split_on_next->next = current_event.parent;

            current_event.parent->active_distance = -1;
            current_event.parent2->active_distance = -1;
            current_event.parent->prev->active_distance = -1;
            current_event.parent2->next->active_distance = -1;

            find_edge_event(current_event.parent->prev, current_event.parent, event_queue, domain);
            find_edge_event(current_event.parent, current_event.parent->next, event_queue, domain);
            find_edge_event(current_event.parent2, current_event.parent2->next, event_queue, domain);
            find_edge_event(current_event.parent2->next, current_event.parent2->next->next, event_queue, domain);
            find_split_event(current_event.parent->prev, current_event.parent, event_queue);
            find_split_event(current_event.parent2, current_event.parent2->next, event_queue);
            print_ring(current_event.parent);
            print_ring(current_event.parent2);
            return;
          }
          if(abs(distance_squared_to_line(current_event.point, current_event.split_on_next->edge) -
                 distance_squared_to_line(current_event.point, current_event.split_on_next->prev->edge)) < 1e-6) {
            //event point is on the boundary of the "zone" of the opposite edge
            std::cout << "pseudo split 2\n";
            skeleton.push_back(new skeleton_node(current_event.point));
            current_event.parent->parent_node->add_edge(skeleton.back());
            std::cout << current_event.parent->parent_node->label << "->" << skeleton.back()->label << std::endl;
            skeleton.back()->add_edge(current_event.parent->parent_node);
            current_event.parent->parent_node = skeleton.back();
            
            skeleton.push_back(new skeleton_node(current_event.point));
            current_event.split_on_next->parent_node->add_edge(skeleton.back());
            std::cout << current_event.split_on_next->parent_node->label << "->" << skeleton.back()->label << std::endl;
            skeleton.back()->add_edge(current_event.split_on_next->parent_node);
            current_event.split_on_next->parent_node = skeleton.back();
            
            

            current_event.parent->prev = current_event.split_on_next->prev;
            current_event.parent2->next = current_event.split_on_next;
            current_event.split_on_next->prev->next = current_event.parent;
            current_event.split_on_next->prev = current_event.parent2;
            find_edge_event(current_event.parent->prev, current_event.parent, event_queue, domain);
            find_edge_event(current_event.parent, current_event.parent->next, event_queue, domain);
            find_edge_event(current_event.parent2, current_event.parent2->next, event_queue, domain);
            find_edge_event(current_event.parent2->next, current_event.parent2->next->next, event_queue, domain);
            print_ring(current_event.parent);
            print_ring(current_event.parent2);
            return;
          }
          //create one new skeleton node with the event parent future intersection's parent-node connected to it
          skeleton.push_back(new skeleton_node(current_event.point));
          current_event.parent->parent_node->add_edge(skeleton.back());
          std::cout << current_event.parent->parent_node->label << "->" << skeleton.back()->label << std::endl;
          skeleton.back()->add_edge(current_event.parent->parent_node);
          current_event.parent->parent_node = skeleton.back();

          //event parent node is not dead
          //create one new future intersection associated with the with a copy of the "opposite" edge as its edge
          //that split edge will appear in both rings
          //the future intersection of the opposite edge gets linked to the parent of the split event as its new prev
          //the newly created future intersection gets linked to the old prev of the parent of the split even as its new next
          future_intersection* new_active = new future_intersection();
          all_faces.push_back(new_active);
          new_active->edge = current_event.split_on_next->edge;
          new_active->prev = current_event.parent->prev;
          new_active->next = current_event.split_on_next->next;

          //link new future intersection into the circular list of future intersections
          //associated with splits of this edge
          new_active->next_split_edge = current_event.split_on_next->next_split_edge;
          if(new_active->next_split_edge == 0x0)
            new_active->next_split_edge = current_event.split_on_next;
          std::cout << "next split edge " << new_active->next_split_edge << std::endl;
          current_event.split_on_next->next_split_edge = new_active;
          std::cout << "next split edge " << new_active << std::endl;

          new_active->parent_node = skeleton.back();
          new_active->active_distance = -1;
          new_active->next->prev = new_active;
          current_event.parent->prev->next = new_active;
          current_event.parent->prev = current_event.split_on_next;
          current_event.parent->prev = current_event.split_on_next;
          current_event.parent->active_distance = -1;
          current_event.split_on_next->next = current_event.parent;

          find_edge_event(new_active, new_active->next, event_queue, domain);
          find_edge_event(new_active->prev, new_active, event_queue, domain);

          find_edge_event(current_event.split_on_next, current_event.split_on_next->next, event_queue, domain);
          find_edge_event(current_event.split_on_next->next, current_event.split_on_next->next->next, event_queue, domain);
          print_ring(current_event.split_on_next);
          print_ring(new_active);

          //find split event here to handle ties between reflex vertices that generate a new reflect vertex
          //find_split_event(current_event.parent->prev, current_event.parent->next, event_queue);
        } else {
          //edge event
          if(current_event.parent->next != current_event.parent2)
            return;
          std::cout << "event " << current_event.point.x() << " " << current_event.point.y() << std::endl;
          std::cout << current_event.parent->parent_node->label << " " << current_event.parent2->parent_node->label << std::endl;
          skeleton.push_back(new skeleton_node(current_event.point));
          //this should implrement couter clockwise order
          current_event.parent->parent_node->add_edge(skeleton.back());
          std::cout << current_event.parent->parent_node->label << "->" << skeleton.back()->label << std::endl;
          current_event.parent->next->parent_node->add_edge(skeleton.back());
          std::cout << current_event.parent->next->parent_node->label << "->" << skeleton.back()->label << std::endl;
          skeleton.back()->add_edge(current_event.parent->next->parent_node);
          std::cout << skeleton.back()->label  << "->" << current_event.parent->next->parent_node->label << std::endl;
          skeleton.back()->add_edge(current_event.parent->parent_node);
          std::cout << skeleton.back()->label << "->" << current_event.parent->parent_node->label << std::endl;
          //parent needs to be removed from the ring because it's face has collapsed at this event
          current_event.parent->prev->next = current_event.parent->next;
          current_event.parent->next->prev = current_event.parent->prev;
          current_event.parent->is_dead = true;
          current_event.parent->next->parent_node = skeleton.back();
          if(current_event.parent->prev->next == current_event.parent->prev->prev) {
            debug1("closed ring");
            //we are done with this ring
            current_event.parent->next->parent_node->add_edge(current_event.parent->prev->parent_node);
            std::cout << current_event.parent->next->parent_node->label << "->" << current_event.parent->prev->parent_node->label << std::endl;
            current_event.parent->prev->is_dead = true;
            current_event.parent->prev->parent_node->add_edge(current_event.parent->next->parent_node);
            std::cout << current_event.parent->prev->parent_node->label << "->" << current_event.parent->next->parent_node->label << std::endl;
            current_event.parent->next->is_dead = true;
            return;
          }
          current_event.parent->active_distance = -1;
          current_event.parent2->active_distance = -1;
          find_edge_event(current_event.parent->prev, current_event.parent->next, event_queue, domain);
          find_edge_event(current_event.parent->next, current_event.parent->next->next, event_queue, domain);

          //we may need to search i-2 and i+2 for edge events

        }
      }

      //main loop
      static void execute_skeleton(std::vector<skeleton_node*>& skeleton, std::vector<future_intersection*>& all_faces,
                                   event_queue_type& event_queue, const rectangle_data<output_coordinate_type>& domain, 
                                   output_coordinate_type bound, bool handle_reflex_vertices = true) {
        while(!event_queue.empty()) {
          event current_event = event_queue.top();
          event_queue.pop();
          if(bound >=0 && current_event.distance > bound)
            break;
          if(!current_event.parent->is_dead &&
             !current_event.parent2->is_dead)
            process_event(current_event, skeleton, all_faces, event_queue, domain);
        }
      }




      //EAGERNESS
      //it would be desirable when a split event occurs to recurse the algorithm into each sub-polygon to eagerly process it
      //holes would need to be split between each sub-polygon and the event queue would need to be split as well
      //the cost of splitting may more than offset the advantages
      //if each future intersection of the sub polygon can be inserted into a new sub event queue they will be invalidated
      //in the main event queue
      //hole discovery could be costly 

      //NOTE ABOUT RESIZING
      //for skeleton based resizing all that is needed is to discard edges of the original polygon that collapse 
      //with bounded edge events and handle reflex vertices of the orignal polygon that participate in bounded split events
      //by associating their edges with the "opposite" split edge for computing bounded angle bisector output polygon vertex
      //Simply terminate skeleton algorithm early and process the set of lists of active 

      //BUILT IN TEST
      template <typename stream_type>
      static bool test_initialize_skeleton(stream_type& stdcout) {
        debug1("test_initialize_skeleton");
        std::string stdendl = "\n";
        rectangle_data<coordinate_type> rect(100, 200, 300, 500);
        rectangle_data<output_coordinate_type> domain(100, 200, 300, 500);
        std::vector<skeleton_node*> skeleton; 
        std::vector<future_intersection*> all_faces;
        event_queue_type event_queue;
        initialize_skeleton(skeleton, all_faces, event_queue, rect);
        if(skeleton.size() == 4 && all_faces.size() == 4) {
          stdcout << event_queue.size() << stdendl;
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr) << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ((*itr)->linked_nodes[0]) << " " <<
              ((*itr)->linked_nodes[1]) << " " << 
              ((*itr)->linked_nodes[2]) << "\n";
          }
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("0"))  << "\n";
          }
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();
          {
          point_data<coordinate_type> pts[] = {point_data<coordinate_type>(120, 200),
                                               point_data<coordinate_type>(300, 200),
                                               point_data<coordinate_type>(300, 500),
                                               point_data<coordinate_type>(100, 500),
                                               point_data<coordinate_type>(100, 220),
          };
          polygon_data<coordinate_type> poly(pts, pts+5);
          stdcout << "five point case\n";
          initialize_skeleton(skeleton, all_faces, event_queue, poly);
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("0"))  << "\n";
          }
          }
          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();

          {
          point_data<coordinate_type> pts[] = {point_data<coordinate_type>(120, 200),
                                               point_data<coordinate_type>(280, 200),
                                               point_data<coordinate_type>(300, 220),
                                               point_data<coordinate_type>(300, 480),
                                               point_data<coordinate_type>(280, 500),
                                               point_data<coordinate_type>(120, 500),
                                               point_data<coordinate_type>(100, 480),
                                               point_data<coordinate_type>(100, 220),
          };
          polygon_data<coordinate_type> poly(pts, pts+8);
          stdcout << "eight point case\n";
          initialize_skeleton(skeleton, all_faces, event_queue, poly);
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("0"))  << "\n";
          }
          }

          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();
          {
          point_data<coordinate_type> pts[] = {point_data<coordinate_type>(120, 200),
                                               point_data<coordinate_type>(280, 200),
                                               point_data<coordinate_type>(300, 220),
                                               point_data<coordinate_type>(300, 480-100),
                                               point_data<coordinate_type>(280, 500-100),
                                               point_data<coordinate_type>(120, 500-100),
                                               point_data<coordinate_type>(100, 480-100),
                                               point_data<coordinate_type>(100, 220),
          };
          polygon_data<coordinate_type> poly(pts, pts+8);
          stdcout << "co-circular eight point case\n";
          initialize_skeleton(skeleton, all_faces, event_queue, poly);
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("0"))  << "\n";
          }
          }

          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();



          {
          point_data<coordinate_type> pts[] = {point_data<coordinate_type>(100, 200),
                                               point_data<coordinate_type>(150, 220),
                                               point_data<coordinate_type>(200, 200),
                                               point_data<coordinate_type>(180, 250),
                                               point_data<coordinate_type>(200, 300),
                                               point_data<coordinate_type>(150, 280),
                                               point_data<coordinate_type>(100, 300),
                                               point_data<coordinate_type>(120, 250),
          };
          polygon_data<coordinate_type> poly(pts, pts+8);
          stdcout << "ninja star\n";
          initialize_skeleton(skeleton, all_faces, event_queue, poly);
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("0"))  << "\n";
          }
          }

          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();

          {
          point_data<coordinate_type> pts[] = {point_data<coordinate_type>(100, 200),
                                               point_data<coordinate_type>(140, 240),
                                               point_data<coordinate_type>(200, 200),
                                               point_data<coordinate_type>(160, 240),
                                               point_data<coordinate_type>(200, 300),
                                               point_data<coordinate_type>(160, 260),
                                               point_data<coordinate_type>(100, 300),
                                               point_data<coordinate_type>(140, 260),
          };
          polygon_data<coordinate_type> poly(pts, pts+8);
          stdcout << "evil ninja star\n";
          initialize_skeleton(skeleton, all_faces, event_queue, poly);
          std::cout << event_queue.size() << " GREP\n";
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("0"))  << "\n";
          }
          }

          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();

          {
          point_data<coordinate_type> pts[] = {point_data<coordinate_type>(100, 200),
                                               point_data<coordinate_type>(300, 200),
                                               point_data<coordinate_type>(300, 500),
                                               point_data<coordinate_type>(100, 500),
                                               point_data<coordinate_type>(290, 350),
          };
          polygon_data<coordinate_type> poly(pts, pts+5);
          stdcout << "concave polygon\n";
          initialize_skeleton(skeleton, all_faces, event_queue, poly);
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("0"))  << "\n";
          }
          }

          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();

          {
          point_data<coordinate_type> pts[] = {point_data<coordinate_type>(100, 200),
                                               point_data<coordinate_type>(300, 200),
                                               point_data<coordinate_type>(300, 500),
                                               point_data<coordinate_type>(100, 500),
                                               point_data<coordinate_type>(100, 490),
                                               point_data<coordinate_type>(120, 490),
                                               point_data<coordinate_type>(100, 489),
                                               point_data<coordinate_type>(100, 450),
                                               point_data<coordinate_type>(122, 450),
                                               point_data<coordinate_type>(100, 449),
                                               point_data<coordinate_type>(100, 420),
                                               point_data<coordinate_type>(120, 420),
                                               point_data<coordinate_type>(100, 419),
          };
          polygon_data<coordinate_type> poly(pts, pts+10);
          stdcout << "multiple split\n";
          initialize_skeleton(skeleton, all_faces, event_queue, poly);
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("null"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("null"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("null"))  << "\n";
          }
          }

          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();

          {
          point_data<coordinate_type> pts[] = {point_data<coordinate_type>(100, 201),
                                               point_data<coordinate_type>(120, 220),
                                               point_data<coordinate_type>(101, 200),
                                               point_data<coordinate_type>(299, 200),
                                               point_data<coordinate_type>(280, 220),
                                               point_data<coordinate_type>(300, 201),
                                               point_data<coordinate_type>(300, 500),
                                               point_data<coordinate_type>(100, 500),
          };
          polygon_data<coordinate_type> poly(pts, pts+8);
          stdcout << "2nd order reflex vertex\n";
          initialize_skeleton(skeleton, all_faces, event_queue, poly);
          execute_skeleton(skeleton, all_faces, event_queue, domain, 1000);
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) {
            std::cout << (*itr)->label << " " << (*itr)->vertex.x() << " " << (*itr)->vertex.y() << " " <<
              ( ((*itr)->linked_nodes[0]) ? (*itr)->linked_nodes[0]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[1]) ? (*itr)->linked_nodes[1]->label : std::string("0"))  << " " <<
              ( ((*itr)->linked_nodes[2]) ? (*itr)->linked_nodes[2]->label : std::string("0"))  << "\n";
          }
          }
          for(typename std::vector<future_intersection*>::iterator itr = all_faces.begin();
              itr != all_faces.end(); ++itr)
            delete *itr;
          all_faces.clear();
          for(typename std::vector<skeleton_node*>::iterator itr = skeleton.begin();
              itr != skeleton.end(); ++itr) 
            delete *itr;
          skeleton.clear();

          return true;
        }
        return false;
      }

    };
  }
}

#endif


