//
//  menuViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#define   DEGREES_TO_RADIANS(degrees)  ((3.14159265359 * degrees)/ 180)
#import "menuViewController.h"

@interface menuViewController ()
@property (weak, nonatomic) IBOutlet UILabel *user;

@end

@implementation menuViewController
@synthesize drawpad;


- (void)viewDidLoad
{
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    
    //fetch username from server
    _user.text = @"Joel Sandberg";
    
    
    
    DrawView* drawableView = [[[DrawView alloc] initWithFrame:CGRectMake(0,0,320,50)] autorelease];
    drawableView.drawBlock = ^(UIView* v,CGContextRef context)
    {
        CGPoint startPoint = CGPointMake(0,v.bounds.size.height-1);
        CGPoint endPoint = CGPointMake(v.bounds.size.width,v.bounds.size.height-1);
        
        CGContextSetStrokeColorWithColor(context, [UIColor grayColor].CGColor);
        CGContextSetLineWidth(context, 1);
        CGContextMoveToPoint(context, startPoint.x + 0.5, startPoint.y + 0.5);
        CGContextAddLineToPoint(context, endPoint.x + 0.5, endPoint.y + 0.5);
        CGContextStrokePath(context);
    };
    [self.view addSubview:drawableView];
    
    
    /*
    
    UIBezierPath *aPath = [UIBezierPath bezierPathWithArcCenter:CGPointMake(150, 150)
                                                         radius:75
                                                     startAngle:0
                                                       endAngle:DEGREES_TO_RADIANS(135)
                                                      clockwise:YES];
    
    UIGraphicsBeginImageContext(self.view.frame.size);
    [[UIColor blackColor] setStroke];
    [aPath fill];
    self.drawpad.image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    
    UIBezierPath *path = [[UIBezierPath alloc] init];
    [path moveToPoint:CGPointMake(50.0f, 50.0f)];
    [path addLineToPoint:CGPointMake(270.0f, 50.0f)];
    [path addLineToPoint:CGPointMake(270.0f, 500.0f)];
    [path addLineToPoint:CGPointMake(50.0f, 500.0f)];
    [path closePath];
    UIGraphicsBeginImageContext(self.view.frame.size);
    path.lineCapStyle = kCGLineCapRound;
    path.lineWidth = 1.0f;
    [[UIColor blackColor] setStroke];
    [[UIColor redColor] setFill];
    [path stroke];
    [path fill];
    self.drawpad.image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //[self.view addSubview:self.drawpad];
    */
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (UIBezierPath *)createArcPath
{
    UIBezierPath *aPath = [UIBezierPath bezierPathWithArcCenter:CGPointMake(150, 150)
                                                         radius:75
                                                     startAngle:0
                                                       endAngle:DEGREES_TO_RADIANS(135)
                                                      clockwise:YES];
    
    
    return aPath;

}


/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

@end
